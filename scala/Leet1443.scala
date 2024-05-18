object Leet1443 extends App {

  import scala.collection.mutable

  object Solution {
    def minTime(n: Int, edges: Array[Array[Int]], hasApple: List[Boolean]): Int = {
      val graph = mutable.Map.empty[Int, List[Int]].withDefaultValue(List())
      // build graph from bigger nodes to smaller nodes
      edges.foreach {
        case Array(from, to) =>
          graph(to) = from :: graph(to)
          graph(from) = to :: graph(from)
      }
      // we shouldn't visit same node twice
      val visited = mutable.Set.empty[Int]
      dfs(0, graph, hasApple.toArray, visited, 0)
    }

    def dfs(
             node: Int,
             graph: mutable.Map[Int, List[Int]],
             apples: Array[Boolean],
             visited: mutable.Set[Int],
             cost: Int): Int = {

      visited += node
      val childrenCost = graph(node).collect {
        case adjNode if !visited.contains(adjNode) =>
          dfs(adjNode, graph, apples, visited, 2)
      }.sum

      // result is either cost of apple itself + children or zero
      if (childrenCost == 0 && !apples(node)) {
        0
      } else {
        childrenCost + cost
      }
    }
  }
}
