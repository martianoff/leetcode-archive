

object Leet198 extends App {

  object Solution {
    def rob(nums: Array[Int]): Int = {
      val dp = Array.fill(nums.length)(0)
      nums.indices.foreach {
        case i if i == 0 =>
          dp(i) = nums(i)
        case i if i == 1 =>
          dp(i) = nums(i) max dp(i - 1)
        case i =>
          // rob a house or skip the house
          dp(i) = (nums(i) + dp(i - 2)) max dp(i - 1)
      }
      dp.last
    }
  }

}
