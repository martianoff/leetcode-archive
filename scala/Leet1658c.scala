object Leet1658c extends App {

  object Solution {
    def minOperations(nums: Array[Int], x: Int): Int = {
      val requiredInternalIntervalSum = nums.sum - x
      // search for longest requiredInternalIntervalSum
      var left = 0
      var right = 0
      var sum = nums(0)
      if (sum == requiredInternalIntervalSum) {
        return nums.length - 1
      }
      if (sum == x) {
        return 1
      }
      var length = 1
      var max = -1
      while (left < nums.length - 1 && (right < nums.length - 1 || sum > requiredInternalIntervalSum)) {
        // move the right border of the window if sum is below than required
        if (sum <= requiredInternalIntervalSum) {
          right += 1
          length += 1
          sum += nums(right)
        } else {
          // move the left border of the window if sum is greater than required
          sum -= nums(left)
          left += 1
          length -= 1
        }
        if (sum == requiredInternalIntervalSum) {
          max = max max length
        }
        // adjust right to make correct sliding
        if (right < left) {
          right += 1
          length += 1
          sum += nums(right)
        }
        if (sum == requiredInternalIntervalSum) {
          max = max max length
        }
      }
      if (max == -1) {
        -1
      } else {
        nums.length - max
      }
    }
  }

}
