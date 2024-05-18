object Leet2008 extends App {

  import scala.collection.mutable

  object Solution {
    def maxTaxiEarnings(n: Int, rides: Array[Array[Int]]): Long = {
      val ridesByEnd = mutable.PriorityQueue.from(rides)(
        Ordering.by((item: Array[Int]) => item(1)).reverse
      )
      val maxAtPoint = Array.fill(n + 1)(0L)
      (1 to n).foreach {
        v =>
          while (ridesByEnd.nonEmpty && ridesByEnd.head(1) == v) {
            ridesByEnd.dequeue() match {
              case Array(start, end, tip) =>
                val value = (end - start + tip)
                maxAtPoint(end) =
                // either continue prev ride or start one of new rides
                  (value + maxAtPoint(start)) max maxAtPoint(end)
            }
          }
          maxAtPoint(v) = maxAtPoint(v - 1) max maxAtPoint(v)
      }
      maxAtPoint(n)
    }
  }

  //
  //0,0,0,0,0,6,6,6,6,6
  //0,0,0,0,0,0,0,0,0,9 prevScore max (max(i-start) + newScore)
  //0,0,0,0,0,0,0,0,0,9,9,14
  //0,0,0,0,0,0,0,0,0,9,9,14 (12 < 14)
  // should be 20
  println(Solution.maxTaxiEarnings(20, Array(Array(1, 6, 1), Array(3, 10, 2), Array(10, 12, 3), Array(11, 12, 2), Array(12, 15, 2), Array(13, 18, 1))))
  // should be 7
  println(Solution.maxTaxiEarnings(5, Array(Array(2, 5, 4), Array(1, 5, 1))))


}
