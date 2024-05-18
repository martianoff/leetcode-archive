import scala.collection.mutable

object Leet1376 extends App {

  object Solution {
    def numOfMinutes(n: Int, headID: Int, manager: Array[Int], informTime: Array[Int]): Int = {
      val employeesByManager = manager.zipWithIndex.groupBy(_._1).map {
        case (managerId, list) => (managerId, list.map(_._2).toList)
      }
      var totalTime: Int = 0
      val queue = mutable.Queue.empty[(Int, Int)]
      queue.enqueue((headID, informTime(headID)))
      while (queue.nonEmpty) {
        queue.dequeue() match {
          case (emplId, timeToInform) =>
            totalTime = timeToInform max totalTime
            employeesByManager.get(emplId) match {
              case Some(list) => queue.enqueueAll(list.map(id => (id, timeToInform + informTime(id))))
              case _ =>
            }
        }
      }
      totalTime
    }
  }

}
