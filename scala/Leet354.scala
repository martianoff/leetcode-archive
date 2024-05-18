object Leet354 extends App {

  import scala.collection.Searching._

  object Solution {
    def maxEnvelopes(envelopes: Array[Array[Int]]): Int = {
      val envelopesSortedByWidth = envelopes.sortWith {
        case (Array(w1, h1), Array(w2, h2)) =>
          if (w1 == w2) {
            h1 > h2
          } else {
            w1 < w2
          }
      }
      longestIncreasingSubsequence(envelopesSortedByWidth.map(_(1)))
    }

    // leetcode #300
    def longestIncreasingSubsequence(nums: Array[Int]): Int = {
      var longestIncreasing = Vector[Int]()
      longestIncreasing = longestIncreasing.appended(nums(0))
      (1 until nums.length).foreach {
        // number increases subsequence
        case i if nums(i) > longestIncreasing.last =>
          longestIncreasing = longestIncreasing.appended(nums(i))
        case i =>
          longestIncreasing.search(nums(i)) match {
            case InsertionPoint(insertionPoint) =>
              longestIncreasing = longestIncreasing.updated(insertionPoint, nums(i))
            // doesn't increase
            case Found(foundPoint) =>
          }
      }
      longestIncreasing.length
    }
  }

}
