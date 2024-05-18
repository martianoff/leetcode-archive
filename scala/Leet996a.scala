object Leet996a extends App {

  import scala.math._
  import scala.collection.mutable

  object Solution {
    def numSquarefulPerms(nums: Array[Int]): Int = {
      dfs(nums, -1)
    }

    def dfs(nums: Array[Int], pos: Int): Int = {
      if (pos == nums.length - 1) {
        return 1
      }
      val dedupSet = mutable.Set.empty[Int]
      // start from pos+1 so we can skip used elements
      (pos + 1 until nums.length).foldLeft(0) {
        case (total, i) =>
          if (!dedupSet.contains(nums(i)) && (pos == -1 || isPerfectSquare(nums(i) + nums(pos)))) {
            // deduplication
            dedupSet += nums(i)
            // move each element to the pos+1 to avoid reusing them again before calling dfs
            val tmp = nums(i)
            nums(i) = nums(pos + 1)
            nums(pos + 1) = tmp
            total + dfs(
              nums = nums.clone,
              pos = pos + 1)
          } else total
      }
    }

    private def isPerfectSquare(num: Int) = {
      sqrt(num) % 1 == 0
    }
  }



  //[89,72,71,44,50,72,26,79,33,27,84]
}
