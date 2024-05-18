import scala.collection.mutable

object Leet2187a extends App {

  import scala.util.chaining._

  object Solution {
    def minimumTime(time: Array[Int], totalTrips: Int): Long = {
      check(
        moment = 1,
        time = time,
        totalTrips = totalTrips,
        trips = Array.fill(time.length)(0))
    }

    private def check(moment: Int, time: Array[Int], totalTrips: Int, trips: Array[Int]): Int = {
      time.zipWithIndex.map {
          case (v, i) if moment % v == 0 =>
            trips(i) + 1
          case (v, i) =>
            trips(i)
        }
        //.tap(trips => println(s"[${trips.mkString(",")}]"))
        .pipe {
          trips =>
            if (trips.sum >= totalTrips) {
              return moment
            }
            check(
              moment + 1,
              time = time,
              totalTrips = totalTrips,
              trips = trips)
        }
    }
  }

}
