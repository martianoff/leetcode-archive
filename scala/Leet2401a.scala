object Leet2401a extends App {

  import scala.collection.mutable

  object Solution {
    def longestNiceSubarray(nums: Array[Int]): Int = {
      var maximum_length = 1

      var current_group = 0
      var left = 0

      nums.indices.foreach {
        right =>
          // If the number at the right point is safe to include, include it in the group and update the maximum length.
          if ((nums(right) & current_group) == 0) {
            current_group |= nums(right)
            maximum_length = maximum_length max (right - left + 1)
          } else {
            // Shrink the window until the number at the right pointer is safe to include.
            while (left < right && (nums(right) & current_group) != 0) {
              current_group &= (~nums(left))
              left += 1
            }
            // Include the number at the right pointer in the group.
            current_group |= nums(right)
          }
      }

      maximum_length
    }
  }

  println(Solution.longestNiceSubarray(Array(1, 3, 8, 48, 10)))


}
