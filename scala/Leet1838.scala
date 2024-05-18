object Leet1838 extends App {
  object Solution {
    def maxFrequency(nums: Array[Int], k: Int): Int = {
      nums.sortInPlace
      var left = 0
      var right = 0
      var sum = nums(0)
      var ans = 1
      // move left
      (0 until nums.length).foreach {
        left =>
          // move right if we can extend interval up to nums(right)
          while (right < nums.length && (right - left + 1) * nums(right) - sum <= k) {
            ans = ans max (right - left + 1)
            right += 1
            if (right < nums.length) {
              sum += nums(right)
            }
          }
          sum -= nums(left)
      }
      ans
    }
  }
}
