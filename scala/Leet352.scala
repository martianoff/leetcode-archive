object Leet352 extends App {

  object Solution {

    import scala.util.chaining._
    import scala.collection.mutable

    class SummaryRanges() {

      private val storage = mutable.TreeMap.empty[Int, Interval]

      def addNum(value: Int) {
        val interval = Interval(value, value)
        // if a new point is within existing intervals don't merge anything
        if (overlaps(interval)) {
          return
        }
        storeInterval(interval)
        concatIntervals(
          concatIntervals(
            getIntervalByEnd(interval.start - 1),
            Some(interval)
          ),
          getIntervalByStart(interval.end + 1)
        )
      }

      def overlaps(interval: Interval): Boolean = {
        (storage.to(interval.start).lastOption, storage.from(interval.end).headOption) match {
          // between start and end points
          case (Some((_, prevInterval)), _) if prevInterval.start <= interval.start && prevInterval.end >= interval.end => true
          // at the start point of existing interval
          case (_, Some((_, nextInterval))) if nextInterval.start == interval.start => true
          // at the end point of existing interval
          case (Some((_, prevInterval)), _) if prevInterval.end == interval.end => true
          case (_, _) => false
        }
      }

      private def getIntervalByEnd(point: Int): Option[Interval] = {
        storage.get(point)
      }

      private def getIntervalByStart(point: Int): Option[Interval] = {
        storage.get(point)
      }

      private def concatIntervals(interval1: Option[Interval], interval2: Option[Interval]): Option[Interval] = {
        (interval1, interval2) match {
          case (None, None) => None
          case (Some(i), None) => Some(i)
          case (None, Some(i)) => Some(i)
          case (Some(i1), Some(i2)) => {
            removeInterval(i1)
            removeInterval(i2)
            Interval(i1.start min i2.start, i1.end max i2.end).tap {
              storeInterval
            }.pipe(Some(_))
          }
        }
      }

      private def storeInterval(interval: Interval) = {
        storage += (interval.start -> interval)
        storage += (interval.end -> interval)
      }

      private def removeInterval(interval: Interval) = {
        storage -= interval.start
        storage -= interval.end
      }

      def getIntervals(): Array[Array[Int]] = {
        storage.collect {
          case (point, interval) if point == interval.start => interval.toArray
        }.toArray
      }

      case class Interval(start: Int, end: Int) {
        def toArray = Array(start, end)
      }

    }

    /**
     * Your SummaryRanges object will be instantiated and called as such:
     * var obj = new SummaryRanges()
     * obj.addNum(value)
     * var param_2 = obj.getIntervals()
     */
  }

}
