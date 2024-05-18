import scala.collection.mutable

object Leet751 extends App {

  object Solution {
    def ipToCIDR(ip: String, n: Int): List[String] = {
      val ipLong = ipToLong(ip)
      // build graph of possible choices
      val graph = mutable.Map.empty[Long, Set[Cidr]].withDefaultValue(Set())
      populateCidrOptions(ipLong, graph, ipLong + n.toLong)
      val queue = mutable.Queue.empty[(Long, List[String])]
      queue.enqueue((ipLong, List()))
      val cache = mutable.Set.empty[Long]
      // bfs with cache
      while (queue.nonEmpty) {
        queue.dequeue() match {
          case (prevIp, path) if !cache.contains(prevIp) =>
            cache += prevIp
            if (prevIp - ipLong == n) {
              return path.reverse
            }
            // pick next, our next turn will always be maxIp + 1
            graph(prevIp).foreach {
              case Cidr(minIp, maxIp, cidr) if !cache.contains(maxIp + 1) =>
                queue.enqueue((maxIp + 1, cidr :: path))
              case _ =>
            }
          case _ =>
        }
      }
      List()
    }

    def populateCidrOptions(ipLong: Long, graph: mutable.Map[Long, Set[Cidr]], target: Long): Unit = {
      // do not explore same paths twice
      if (graph.contains(ipLong)) {
        return
      }
      var maxIp = ipLong
      var minIp = ipLong
      var mask = 0
      var counter = 32
      // ignore paths to bigger ips
      while (maxIp <= target) {
        maxIp = (ipLong | mask)
        minIp = ipLong & ~mask
        // ignore paths to smaller ips
        if (minIp == ipLong) {
          graph(ipLong) = graph(ipLong) + Cidr(minIp, maxIp, ipLongToIp(minIp) + "/" + counter)
          // build sub paths
          populateCidrOptions(maxIp + 1, graph, target)
        }
        if (mask == 0) {
          mask = 1
        } else {
          mask = (mask << 1) + 1
        }
        counter -= 1
      }
    }

    def ipLongToIp(ip: Long): String = {
      (24 to 0 by -8).map { offset =>
        (ip >> offset) & 0xff // read just last byte
      }.mkString(".")
    }

    def ipToLong(ip: String): Long = {
      ip.split('.') match {
        case Array(a, b, c, d) =>
          d.toLong + (c.toLong << 8) + (b.toLong << 16) + (a.toLong << 24)
      }
    }

    case class Cidr(minIp: Long, maxIp: Long, cidr: String)

  }

  println(Solution.ipToLong("1.2.3.4")) //16909060
  println(Solution.ipLongToIp(16909060)) //"1.2.3.4"


}
