object Leet974 extends App {

  object Solution {
    def subarraysDivByK(nums: Array[Int], k: Int): Int = {
      // extra 0 element in prefixSum required to track entire substring:
      // the last element minus 0 gives a sum of entire substring
      val prefixSum = nums.zipWithIndex.foldLeft(Array.fill(nums.length + 1)(0)) {
        case (prefixSum, (num, index)) =>
          prefixSum(index + 1) = num + prefixSum(index)
          prefixSum
      }
      prefixSum.indices.map { i =>
        (i + 1 until prefixSum.length).map {
          case j if (prefixSum(j) - prefixSum(i)) % k == 0 => 1
          case _ => 0
        }.sum
      }.sum
    }
  }

}
