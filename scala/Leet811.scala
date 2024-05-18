import scala.collection.mutable

object Leet811 extends App {

  object Solution {
    def subdomainVisits(cpdomains: Array[String]): List[String] = {
      val scoresByDomain = mutable.Map.empty[String, Int]
      cpdomains.map(_.split(' ').toSeq).map {
        case Seq(visits, host) =>
          host.split('.').foldRight(List.empty[String]) {
            case (newDomainPart, oldDomainParts) =>
              val domainNameParts = if (oldDomainParts.isEmpty) {
                List(newDomainPart)
              } else {
                newDomainPart :: "." :: oldDomainParts
              }
              val domainNameStr = domainNameParts.mkString
              scoresByDomain(domainNameStr) = scoresByDomain.get(domainNameStr) match {
                case Some(oldVisits) => oldVisits + visits.toInt
                case None => visits.toInt
              }
              domainNameParts
          }
      }
      scoresByDomain.map {
        case (domain, score) => s"${score} ${domain}"
      }.toList
    }
  }

  println(Solution.subdomainVisits(Array("9001 discuss.leetcode.com")))
}