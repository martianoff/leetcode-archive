object Leet992 extends App {

  import scala.collection.mutable

  object Solution {
    def subarraysWithKDistinct(nums: Array[Int], k: Int): Int = {
      //[1,2,1,2,3]
      //1       | move right <= k
      //1,2     | move right <= k
      //1,2,1   | move right <= k
      //1,2,1,2 | move left because met k, move right to the pointer where <= k
      //2,1     | move right <= k
      //2,1,2   | move left because met k, move right to the pointer where <= k
      //1,2     | move left because met k, move right to the pointer where <= k
      //2,3     |
      var left = 0
      var right = -1
      val selected = mutable.Map.empty[Int, Int].withDefaultValue(0)
      var total = 0
      while (left < nums.length) {
        // move right if possible and new substring will satisfy conditions <= k
        if ((right < nums.length - 1) &&
          ((selected.size <= k && selected.contains(nums(right + 1))) ||
            (selected.size < k && !selected.contains(nums(right + 1))))) {
          right += 1
          selected(nums(right)) += 1
        } else {
          // no more way to build a substring
          if (selected.size < k) {
            return total
          }
          // otherwise move left
          selected(nums(left)) -= 1
          if (selected(nums(left)) == 0) {
            selected -= nums(left)
          }
          left += 1
          val currentSize = selected.size
          // move right to the left most position position
          while (right > left && selected.size == currentSize) {
            selected(nums(right)) -= 1
            if (selected(nums(right)) == 0) {
              selected -= nums(right)
            }
            right -= 1
          }
        }
        if (selected.size == k) {
          total += 1
        }
      }
      total
    }
  }
}
