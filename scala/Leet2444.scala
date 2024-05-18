object Leet2444 extends App {

  object Solution {
    def countSubarrays(nums: Array[Int], minK: Int, maxK: Int): Long = {
      var minPos = Option.empty[Int]
      var maxPos = Option.empty[Int]
      var leftBound = Option.empty[Int]
      nums.indices.map { i =>
        // set border if out of range
        if (nums(i) < minK || nums(i) > maxK) {
          leftBound = Some(i)
        }
        // remember pos of min
        if (nums(i) == minK) {
          minPos = Some(i)
        }
        // remember pos of max
        if (nums(i) == maxK) {
          maxPos = Some(i)
        }
        (minPos, maxPos) match {
          case (Some(minPos), Some(maxPos)) => (0 max ((maxPos min minPos) - leftBound.getOrElse(-1))).toLong
          case _ => 0L
        }
      }.sum
    }
  }

}
