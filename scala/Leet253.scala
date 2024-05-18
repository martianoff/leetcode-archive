object Leet253 extends App {

  import scala.collection.mutable

  object Solution {
    def minMeetingRooms(intervals: Array[Array[Int]]): Int = {
      // sort by start time
      intervals.sortInPlaceWith {
        case (Array(s1, e1), Array(s2, e2)) => s1 < s2
      }
      val occupiedRooms = mutable.PriorityQueue.empty[Int](Asc)
      var roomsCount = 0
      intervals.foreach {
        case Array(start, end) =>
          // release rooms if they are no longer required (their lease time is less than start of the meeting)
          while (occupiedRooms.headOption.exists(_ <= start)) {
            occupiedRooms.dequeue()
          }
          // hold a new room
          occupiedRooms.enqueue(end)
          roomsCount = roomsCount max occupiedRooms.size
      }
      roomsCount
    }

    object Asc extends Ordering[Int] {
      def compare(a: Int, b: Int): Int = {
        b compare a
      }
    }
  }

}
