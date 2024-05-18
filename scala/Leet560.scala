object Leet560 extends App {

  import scala.util.chaining._

  object Solution {
    def subarraySum(nums: Array[Int], k: Int): Int = {
      nums.foldLeft((0, Map(0 -> 1).withDefaultValue(0), 0)) {
        case ((acc, subarrays, res), n) =>
          (acc + n).pipe { subseqsum =>
            (
              subseqsum,
              subarrays + (subseqsum -> (subarrays(subseqsum) + 1)),
              res + subarrays(subseqsum - k)
            )
          }
      }._3
    }
  }
}