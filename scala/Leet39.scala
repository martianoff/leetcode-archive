import scala.collection.mutable

object Leet39 extends App {
  object Solution {
    def combinationSum(candidates: Array[Int], target: Int): List[List[Int]] = {
      candidates.sortInPlace
      val results = mutable.ListBuffer.empty[List[Int]]
      dfs(candidates, target, 0, candidates.length, results)
      results.toList
    }

    private def dfs(
                     candidates: Array[Int],
                     target: Int,
                     from: Int,
                     to: Int,
                     results: mutable.ListBuffer[List[Int]],
                     path: List[Int] = List(),
                     sum: Int = 0): Unit = {
      if (sum == target) {
        results += path
      } else if (sum < target) {
        (from until to).foreach {
          index =>
            dfs(
              candidates,
              target,
              index,
              candidates.length,
              results,
              candidates(index) :: path,
              sum + candidates(index)
            )
        }
      }
    }
  }
}
