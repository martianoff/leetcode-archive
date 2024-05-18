

object Leet210 extends App {

  import scala.collection.mutable

  object Solution {
    def findOrder(numCourses: Int, prerequisites: Array[Array[Int]]): Array[Int] = {
      val visited = mutable.Set.empty[Int]
      val nodeToIndegree = mutable.Map.empty[Int, Int]
      (0 until numCourses).foreach {
        i => nodeToIndegree(i) = 0
      }
      val graph = mutable.Map.empty[Int, List[Int]].withDefaultValue(List())
      // calculate indegree and build graph
      prerequisites.foreach {
        case Array(to, from) =>
          graph(from) = to :: graph(from)
          nodeToIndegree(to) += 1
      }
      val results = mutable.Queue.empty[Int]
      val traverseQueue = mutable.Queue.empty[Int]
      // start with 0 indegree
      (0 until numCourses).foreach {
        case i if nodeToIndegree(i) == 0 => traverseQueue.enqueue(i)
        case _ =>
      }
      while (traverseQueue.nonEmpty) {
        traverseQueue.dequeue() match {
          case n =>
            visited += n
            results.enqueue(n)
            graph(n).foreach {
              case connectedNode if !visited.contains(connectedNode) =>
                nodeToIndegree(connectedNode) -= 1
                if (nodeToIndegree(connectedNode) == 0) {
                  traverseQueue.enqueue(connectedNode)
                }
              case _ =>
            }
        }
      }
      if (visited.size != numCourses) {
        return Array()
      }
      results.toArray
    }
  }

}
