object Leet494c extends App {

  import scala.collection.mutable
  import scala.util.chaining._

  object Solution {
    def findTargetSumWays(nums: Array[Int], target: Int): Int = {
      val cache = mutable.Map.empty[(Int, Int), Int]
      dfs(nums, target, cache)
    }

    private def dfs(
                     nums: Array[Int],
                     target: Int,
                     cache: mutable.Map[(Int, Int), Int],
                     curItem: Int = 0,
                     sum: Int = 0): Int = {
      if (cache.contains((curItem, sum))) {
        return cache((curItem, sum))
      }
      (if (curItem >= nums.length) {
        if (sum == target)
          1
        else
          0
      } else {
        dfs(nums, target, cache, curItem + 1, sum + nums(curItem)) +
          dfs(nums, target, cache, curItem + 1, sum - nums(curItem))
      }).tap {
        result =>
          cache += ((curItem, sum) -> result)
      }
    }
  }

}
