object Leet1235a extends App {

  import scala.collection.mutable

  object Solution {
    def jobScheduling(startTime: Array[Int], endTime: Array[Int], profit: Array[Int]): Int = {
      val jobs = mutable.PriorityQueue.empty[Job](EndTimeDesc)
      startTime.zipWithIndex.foreach {
        case (startTime, id) =>
          jobs.enqueue(Job(startTime, endTime(id), profit(id)))
      }
      val bestByEndTime = mutable.TreeMap.empty[Int, Int]
      while (jobs.nonEmpty) {
        jobs.dequeue() match {
          case Job(startTime, endTime, profit) =>
            val bestValueSoFar = bestByEndTime.lastOption.map(_._2).getOrElse(0)
            val possibleProfitAtThisEnd = profit + bestByEndTime.rangeTo(startTime).lastOption.map(_._2).getOrElse(0)
            if (possibleProfitAtThisEnd > bestValueSoFar) {
              bestByEndTime(endTime) = possibleProfitAtThisEnd
            }
        }
      }
      bestByEndTime.last._2
    }

    case class Job(startTime: Int, endTime: Int, profit: Int)

    object EndTimeDesc extends Ordering[Job] {
      def compare(a: Job, b: Job) = {
        b.endTime compare a.endTime
      }
    }
  }

}
