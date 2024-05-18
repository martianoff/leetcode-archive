object Leet494 extends App {

  import scala.collection.mutable

  object Solution {
    def findTargetSumWays(nums: Array[Int], target: Int): Int = {
      val results = mutable.Set.empty[List[SignedNumber]]
      dfs(nums, target, results)
      results.size
    }

    private def dfs(
                     nums: Array[Int],
                     target: Int,
                     results: mutable.Set[List[SignedNumber]],
                     path: List[SignedNumber] = List(),
                     curItem: Int = 0,
                     sum: Int = 0): Unit = {
      if (curItem >= nums.length) {
        if (sum == target) {
          results += path
        }
        return
      }
      val num = nums(curItem)
      dfs(nums, target, results, SignedNumber(num, Plus) :: path, curItem + 1, sum + num)
      dfs(nums, target, results, SignedNumber(num, Minus) :: path, curItem + 1, sum - num)
    }

    sealed trait Sign

    case class SignedNumber(number: Int, sign: Sign)

    case object Plus extends Sign

    case object Minus extends Sign
  }

}
