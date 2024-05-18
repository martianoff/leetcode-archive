object Leet759 extends App {

  class Interval(var _start: Int, var _end: Int) {
    var start: Int = _start
    var end: Int = _end
  }

  import scala.collection.mutable

  object Solution {

    def employeeFreeTime(schedule: List[List[Interval]]): List[Interval] = {
      val priorityQueue = mutable.PriorityQueue.from(schedule.flatten)(OrderByStartAsc)
      var prevInterval = Option.empty[Interval]
      val workIntervals = mutable.Queue.empty[Interval]
      // merge all work intervals
      while (priorityQueue.nonEmpty) {
        val interval = priorityQueue.dequeue()
        println(s"${interval.start},${interval.end}")
        prevInterval match {
          // if intersects, extend prevInterval
          case Some(prev) if prev.end >= interval.start => prev.end = interval.end max prev.end
          // else
          case Some(prev) =>
            workIntervals.enqueue(prev)
            prevInterval = Some(interval)
          case None => prevInterval = Some(interval)
        }
      }
      prevInterval.foreach(workIntervals.enqueue(_))
      // find gaps in merged work intervals
      workIntervals.foldLeft(Vector.empty[Interval], Option.empty[Int]) {
        // no known last end
        case ((aggr, None), interval) => (aggr, Some(interval.end))
        // known last end
        case ((aggr, Some(lastEnd)), interval) => (aggr.appended(new Interval(lastEnd, interval.start)), Some(interval.end))
      }._1.toList
    }

    object OrderByStartAsc extends Ordering[Interval] {
      def compare(a: Interval, b: Interval) = {
        b.start compare a.start
      }
    }
  }

}
