object Leet55 extends App {

  object Solution {
    def canJump(nums: Array[Int]): Boolean = {
      (0 until nums.length).foldLeft(0) {
        // find max possible index which we can jump to
        // if current position is reachable
        case (overAllMax, i) if i <= overAllMax => (i + nums(i)) max overAllMax
        // use return to stop recursion as soon as possible
        case (overAllMax, _) => return false
      } >= nums.length - 1
    }
  }

}
