object Leet2150 extends App {
  object Solution {
    def findLonely(nums: Array[Int]): List[Int] = {
      val freqMap = nums.groupBy(v => v).map { case (v, nums) => (v, nums.length) }
      freqMap.collect {
        case (v, count) if count == 1
          && (!freqMap.contains(v - 1))
          && (!freqMap.contains(v + 1)) => v
      }.toList
    }
  }
}
