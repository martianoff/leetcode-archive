import scala.collection.mutable

object Leet1136 extends App {

  import scala.collection.mutable

  object Solution {
    def minimumSemesters(n: Int, relations: Array[Array[Int]]): Int = {
      // build indegree map and graph
      val indegree = mutable.Map.empty[Int, Int]
      (1 to n).foreach {
        v => indegree(v) = 0
      }
      val graph = mutable.Map.empty[Int, List[Int]].withDefaultValue(List())
      relations.foreach {
        case Array(from, to) =>
          indegree(to) += 1
          graph(from) = to :: graph(from)
      }
      // find starting points
      val queue = mutable.Queue.empty[(Int, Int)]
      indegree.foreach {
        case (n, in) if in == 0 => queue.enqueue((n, 1))
        case _ =>
      }
      // explore using topological search
      var semesters = 0
      val needToVisit = mutable.Set.from(1 to n)
      while (queue.nonEmpty) {
        queue.dequeue() match {
          case (n, semester) =>
            needToVisit -= n
            if (needToVisit.isEmpty) {
              return semester
            }
            graph(n).foreach {
              dest =>
                indegree(dest) -= 1
                if (indegree(dest) == 0) {
                  queue.enqueue((dest, semester + 1))
                }
            }
        }
      }
      -1
    }
  }

}
