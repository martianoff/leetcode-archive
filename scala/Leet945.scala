object Leet945 extends App {

  object Solution {
    def minIncrementForUnique(nums: Array[Int]): Int = {
      nums.sortInPlace
      // track down sum of increments and the next allowed number
      nums.foldLeft(0, -1) {
        // violation of sequence
        case ((sum, nextAllowedNumber), num) if num < nextAllowedNumber =>
          (sum + nextAllowedNumber - num, nextAllowedNumber + 1)
        // no violation
        case ((sum, _), num) =>
          (sum, num + 1)
      }._1
    }
  }
}
