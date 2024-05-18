object Leet494b extends App {

  object Solution {
    def findTargetSumWays(nums: Array[Int], target: Int): Int = {
      dfs(nums, target)
    }

    private def dfs(
                     nums: Array[Int],
                     target: Int,
                     curItem: Int = 0,
                     sum: Int = 0): Int = {
      if (curItem >= nums.length) {
        if (sum == target)
          1
        else
          0
      } else {
        dfs(nums, target, curItem + 1, sum + nums(curItem)) +
          dfs(nums, target, curItem + 1, sum - nums(curItem))
      }
    }
  }

}
