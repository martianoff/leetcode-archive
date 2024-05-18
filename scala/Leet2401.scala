object Leet2401 extends App {

  object Solution {
    def longestNiceSubarray(nums: Array[Int]): Int = {
      var left = 0
      var right = 0
      var niceLen = 0
      var maxNiceLen = 0
      var diff = 0
      while (left < nums.length && left <= right) {
        println(s"l${left} r${right}")
        if (right < nums.length && (nums(left) == nums(right) || diff == 0)) {
          right += 1
          diff = diff & nums(right)
          niceLen += 1
        } else {
          diff = diff & ~nums(left)
          left += 1
          niceLen -= 1
        }
        maxNiceLen = niceLen max maxNiceLen
      }
      maxNiceLen
    }
  }

  println(Solution.longestNiceSubarray(Array(1, 3, 8, 48, 10)))


}
