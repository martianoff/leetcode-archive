object Leet41 extends App {
  object Solution {
    def firstMissingPositive(nums: Array[Int]): Int = {
      nums.zipWithIndex.foldLeft(false) {
        case (hasOne, (n, i)) =>
          // remove negatives and zeroes
          if (n <= 0)
            nums(i) = 1
          // search for one
          hasOne || n == 1
      } match {
        // has 1
        case true =>
          // using indices as ordered sequence
          // mark with "-" all values within [1 to length] range
          // the index of the first positive value plus one will be the missing element in the sequence
          nums.zipWithIndex.foreach {
            // value should be in range of [1 to length], and be positive
            case (n, i) if n <= nums.length && n > 0 && nums(n - 1) > 0 =>
              nums(n - 1) *= -1
            case _ =>
          }
          nums
            .zipWithIndex
            .collectFirst { case (n, i) if n >= 0 => i + 1 }
            .getOrElse(nums.length + 1)
        // doesn't have 1
        case false =>
          1
      }
    }
  }
}
