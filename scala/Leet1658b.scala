object Leet1658b extends App {

  object Solution {
    def minOperations(nums: Array[Int], x: Int): Int = {
      val requiredInternalIntervalSum = nums.sum - x
      println(s"requiredInternalIntervalSum=${requiredInternalIntervalSum}")
      nums.foldLeft((0, Set(0), 0)) {
        case ((lastSum, prefixSums, counter), v) =>
          val sumSoFar = lastSum + v
          println(s"prefixSums=${prefixSums} sumSoFar=${sumSoFar}")
          if (prefixSums.contains(sumSoFar - requiredInternalIntervalSum)) {
            return nums.length - counter + 1
          }
          (sumSoFar, prefixSums + sumSoFar, counter + 1)
      }
      -1
    }
  }

}
