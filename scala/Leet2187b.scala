

object Leet2187b extends App {

  object Solution {
    def minimumTime(time: Array[Int], totalTrips: Int): Long = {
      var left: Long = 1L
      var right: Long = time.min * totalTrips.toLong
      while (left < right) {
        val moment = (right + left) / 2
        totalTripsAtMoment(time, moment = moment) match {
          case trips if trips >= totalTrips =>
            right = moment
          case _ =>
            left = moment + 1
        }
      }
      left
    }

    private def totalTripsAtMoment(time: Array[Int], moment: Long): Long = {
      time.foldLeft(0L) {
        case (s, v) => s + moment / v
      }
    }
  }

}
