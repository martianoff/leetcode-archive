

object Leet1011 extends App {

  object Solution {
    def shipWithinDays(weights: Array[Int], days: Int): Int = {
      // ans is between maxWeight and totalWeight
      val totalWeights = weights.sum
      var left = weights.max
      var right = totalWeights
      var result = totalWeights
      while (left <= right) {
        val mid = (left + right) / 2
        val daysWithThisCapacity = minDaysWithCapacity(weights, mid)
        if (daysWithThisCapacity <= days) {
          result = result min mid
        }
        if (daysWithThisCapacity <= days) {
          // we have extra capacity and can work faster
          right = mid - 1
        } else {
          // not enough capacity
          left = mid + 1
        }
      }
      result
    }

    private def minDaysWithCapacity(weights: Array[Int], capacity: Int): Int = {
      weights.foldLeft((1, 0)) {
        // fill at the same day
        case ((days, used), weight) if used + weight <= capacity =>
          (days, used + weight)
        // fill at the next day
        case ((days, used), weight) =>
          (days + 1, weight)
      }._1
    }
  }

}
