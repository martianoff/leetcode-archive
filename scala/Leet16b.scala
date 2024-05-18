object Leet16b extends App {

  import scala.collection.mutable

  object Solution {
    def threeSumClosest(nums: Array[Int], target: Int): Int = {
      nums.sortInPlace
      val size = nums.length
      var bestDiff = Int.MaxValue
      (0 until size).foreach {
        i =>
          var low = i + 1
          var hi = size - 1
          while (low < hi) {
            val sum = nums(i) + nums(low) + nums(hi)
            if ((target - sum).abs < bestDiff.abs) {
              bestDiff = target - sum
              if (bestDiff == 0) {
                return target
              }
            }
            if (sum < target) {
              low += 1
            } else {
              hi -= 1
            }
          }
      }
      target - bestDiff
    }
  }

}
