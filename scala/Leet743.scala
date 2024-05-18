object Leet743 extends App {

  import scala.collection.mutable

  object Solution {
    def networkDelayTime(times: Array[Array[Int]], n: Int, k: Int): Int = {
      // build graph (vertex to edges list)
      val graph = mutable.Map.empty[Int, List[Edge]].withDefaultValue(List.empty[Edge])
      times.foreach {
        case Array(sourceId, targetId, weight) =>
          graph(sourceId) = Edge(targetId, weight) :: graph(sourceId)
      }
      // dijkstra algorithm
      var result = 0
      val priorityQueue = mutable.PriorityQueue.empty[Edge](Asc)
      // start from virtual edge to our source
      priorityQueue.enqueue(Edge(targetId = k, weight = 0))
      val visitedVertices = mutable.Set.empty[Int]
      while (priorityQueue.nonEmpty) {
        priorityQueue.dequeue() match {
          case Edge(sourceId, sourceWeight) if !visitedVertices.contains(sourceId) =>
            result = sourceWeight max result
            visitedVertices += sourceId
            graph(sourceId).collect {
              case Edge(targetId, targetWeight) if !visitedVertices.contains(targetId) =>
                priorityQueue.enqueue(
                  Edge(targetId = targetId, weight = targetWeight + sourceWeight)
                )
            }
          case _ =>
        }
      }
      if (visitedVertices.size == n) {
        result
      } else {
        -1
      }
    }

    case class Edge(targetId: Int, weight: Int)

    object Asc extends Ordering[Edge] {
      def compare(edge1: Edge, edge2: Edge): Int = {
        edge2.weight compare edge1.weight
      }
    }
  }

}
