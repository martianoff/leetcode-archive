object Leet300 extends App {

  import scala.collection.Searching._

  object Solution {
    def lengthOfLIS(nums: Array[Int]): Int = {
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
