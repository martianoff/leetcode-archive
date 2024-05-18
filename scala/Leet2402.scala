object Leet2402 extends App {

  import scala.collection.mutable

  object Solution {
    def mostBooked(n: Int, meetings: Array[Array[Int]]): Int = {
      val meetingQueue = mutable.PriorityQueue.from(meetings.map {
        case Array(start, end) => Meeting(start, end)
      })(ByStartTimeAsc)
      val pendingRooms = mutable.PriorityQueue.empty[Room](ByAvailableAtAsc)
      val availableRooms = mutable.PriorityQueue.from(
        (0 until n).map(i => Room(availableAt = 0, roomNumber = i))
      )(ByRoomIdAsc)
      var meetingsPerRoom = mutable.Map.empty[Int, Int].withDefaultValue(0)
      var time: Long = meetingQueue.head.start.toLong
      while (meetingQueue.nonEmpty) {
        // return pending rooms
        while (pendingRooms.nonEmpty && pendingRooms.head.availableAt <= time) {
          availableRooms.enqueue(pendingRooms.dequeue())
        }
        // we have available rooms room
        while (availableRooms.nonEmpty &&
          // have meetings
          meetingQueue.nonEmpty &&
          // meeting can be started
          time >= meetingQueue.head.start) {
          val meeting = meetingQueue.dequeue()
          val room = availableRooms.dequeue()
          meetingsPerRoom(room.roomNumber) += 1
          pendingRooms.enqueue(room.copy(availableAt = time + meeting.duration.toLong))
        }
        // jump to next start/available slot
        if (meetingQueue.nonEmpty) {
          if (pendingRooms.nonEmpty && availableRooms.isEmpty) {
            time = pendingRooms.head.availableAt max meetingQueue.head.start
          } else {
            time = meetingQueue.head.start
          }
        }
      }
      // output the best result
      meetingsPerRoom.toList.sortWith {
        case ((roomId1, value1), (roomId2, value2)) =>
          if (value2 == value1) {
            roomId2 > roomId1
          } else {
            value2 < value1
          }
      }.head._1
    }

    case class Room(availableAt: Long, roomNumber: Int)

    case class Meeting(start: Int, end: Int) {
      val duration: Int = end - start
    }

    object ByStartTimeAsc extends Ordering[Meeting] {
      def compare(meeting1: Meeting, meeting2: Meeting) = {
        meeting2.start compare meeting1.start
      }
    }

    object ByAvailableAtAsc extends Ordering[Room] {
      def compare(meeting1: Room, meeting2: Room) = {
        meeting2.availableAt compare meeting1.availableAt
      }
    }

    object ByRoomIdAsc extends Ordering[Room] {
      def compare(meeting1: Room, meeting2: Room) = {
        meeting2.roomNumber compare meeting1.roomNumber
      }
    }
  }


}
