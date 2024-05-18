object Leet1192 extends App {

  import scala.collection.mutable

  object Solution {
    var time: Int = -1

    // Tarjan's algorithm
    def criticalConnections(n: Int, connections: List[List[Int]]): List[List[Int]] = {
      time = -1
      val ids = Array.fill(n)(-1)
      val lows = Array.fill(n)(-1)
      val hops = mutable.LinkedHashSet.empty[Int]
      val graph = mutable.Map.empty[Int, List[Int]].withDefaultValue(List())
      connections.foreach {
        case List(from, to) if from != to =>
          graph(from) = to :: graph(from)
          graph(to) = from :: graph(to)
        case _ =>
      }
      // find lows of each vertex
      dfs(0, -1, hops, graph, ids, lows)
      // bridges are any connections where lows between two points are not equal
      connections.collect {
        case List(from, to) if lows(from) != lows(to) =>
          List(from, to)
      }
    }

    private def dfs(
                     from: Int,
                     parent: Int,
                     hops: mutable.LinkedHashSet[Int],
                     graph: mutable.Map[Int, List[Int]],
                     ids: Array[Int],
                     lows: Array[Int]): Unit = {

      time += 1
      ids(from) = time
      lows(from) = time
      hops += from

      graph(from).foreach {
        dest =>
          // run dfs if not yet visited
          if (ids(dest) == -1) {
            dfs(dest, from, hops, graph, ids, lows)
          }
          // backedge detected (loop)
          if (hops.contains(dest) && parent != dest) {
            // propagates lows from next hops
            lows(from) = lows(from) min lows(dest)
          }
      }

      // when we are at a parent of the SCC (strongly connected component), remove hops
      if (ids(from) == lows(from)) {
        while (hops.last != from) {
          lows(hops.last) = ids(from)
          hops -= hops.last
        }
        hops -= from
      }

    }
  }

}
