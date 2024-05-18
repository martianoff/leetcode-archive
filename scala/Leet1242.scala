import scala.concurrent.{ExecutionContext, ExecutionContextExecutor, Future}
import scala.collection.mutable

object Leet1242 extends App {

  /**
   * // This is the HtmlParser's API interface.
   * // You should not implement it, or speculate about its implementation
   * interface HtmlParser {
   * public List<String> getUrls(String url) {}
   * }
   */

  trait HtmlParser {
    def getUrls(url: String): Future[List[String]]

    def getTitle(url: String): Future[String]
  }

  implicit val ec: ExecutionContextExecutor = ExecutionContext.global

  object Solution {
    private val visited = mutable.Set.empty[String]

    def crawl(startUrl: String, htmlParser: HtmlParser): Future[List[String]] = {
      visited.synchronized {
        if (visited.contains(startUrl)) {
          return Future(List())
        }
        visited += startUrl
      }

      val startUrlHost = urlHost(startUrl)
      htmlParser.getUrls(startUrl).flatMap {
        urls =>
          Future.sequence(
              urls
                .collect {
                  case url if urlHost(url) == startUrlHost && !visited.synchronized(visited.contains(url)) => crawl(url, htmlParser)
                }
            )
            .map(startUrl :: _.flatten)
            .map(_.distinct)
      }
    }

    private def urlHost(url: String): String = {
      new java.net.URI(url).getHost
    }
  }


}
