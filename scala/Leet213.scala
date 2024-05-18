

object Leet213 extends App {

  object Solution {
    def rob(nums: Array[Int]): Int = {
      if (nums.length == 1) {
        return nums.head
      }
      lc198(nums.tail) max lc198(nums.init)
    }

    private def lc198(nums: Array[Int]): Int = {
      val dp = Array.fill(3)(0)
      nums.indices.foreach {
        index =>
          index match {
            case i if i == 0 =>
              dp(i) = nums(i)
            case i if i == 1 =>
              dp(i) = nums(i) max dp(i - 1)
            case i =>
              // rob a house or skip the house
              dp(2) = (nums(i) + dp(0)) max dp(1)
          }
          // shift array to left
          if (index >= 2) {
            dp(0) = dp(1)
            dp(1) = dp(2)
          }
      }
      dp((nums.length - 1) min (dp.length - 1))
    }
  }

}
