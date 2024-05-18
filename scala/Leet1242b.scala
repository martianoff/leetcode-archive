import scala.collection.mutable
import scala.concurrent.{Await, ExecutionContext, ExecutionContextExecutor, Future}

object Leet1242b extends App {

  /**
   * // This is the HtmlParser's API interface.
   * // You should not implement it, or speculate about its implementation
   * interface HtmlParser {
   * public List<String> getUrls(String url) {}
   * }
   */

  trait HtmlParser {
    def getUrls(url: String): Future[List[String]]
  }

  implicit val ec: ExecutionContextExecutor = ExecutionContext.global

  object Solution {
    class Crawler(startUrl: String, htmlParser: HtmlParser) {
      private val startUrlHost = urlHost(startUrl)
      private val queue = mutable.Queue.empty[String]
      private val visited = mutable.Set.empty[String]

      def start(): Future[Unit] = {
        queue.synchronized {
          queue.enqueue(startUrl)
        }
        process()
      }

      private def urlHost(url: String): String = {
        new java.net.URI(url).getHost
      }

      private def process(): Future[Unit] = {
        val nextUrl = queue.synchronized {
          if (queue.nonEmpty) {
            queue.dequeue()
          } else {
            return Future.unit
          }
        }
        visited.synchronized {
          if (visited.contains(nextUrl)) {
            return Future(List())
          }
          visited += nextUrl
        }
        htmlParser.getUrls(startUrl).flatMap {
          urls =>
            Future.sequence(
              urls
                .filter(url => urlHost(url) == startUrlHost && !visited.contains(url))
                .map {
                  url =>
                    queue.synchronized {
                      queue.enqueue(url)
                    }
                    process()
                }
            ).map {
              _ =>
            }
        }
      }
    }

    //Await.result((new Crawler("https://google.com")).start())
  }


}
