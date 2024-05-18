object Leet729 extends App {

  import scala.collection.mutable

  class MyCalendar() {

    val storage: mutable.TreeMap[Int, PointType.PointType] = mutable.TreeMap.empty[Int, PointType.PointType]

    def book(start: Int, end: Int): Boolean = {
      // new range contains an event inside
      if (storage.range(start, end).nonEmpty) {
        return false
      }
      storage.until(start).lastOption match {
        // new range is within existing event
        case Some((_, lastPointType)) if lastPointType == PointType.Start =>
          false
        case _ =>
          storage += (start -> PointType.Start)
          storage += (end - 1 -> PointType.End)
          true
      }
    }

  }

  object PointType extends Enumeration {
    type PointType = Value
    val Start, End = Value
  }

}
