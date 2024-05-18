object Leet454 extends App {

  object Solution {
    def fourSumCount(nums1: Array[Int], nums2: Array[Int], nums3: Array[Int], nums4: Array[Int]): Int = {
      nums3.flatMap {
          v3 =>
            nums4.map {
              v4 => v4 + v3
            }
        }
        .groupBy(v => v)
        .map {
          case (k, tuples) => (k, tuples.length)
        } match {
        case targetMap =>
          nums1.map {
            v1 =>
              nums2.collect {
                case v2 if targetMap.contains(-v1 - v2) => targetMap(-v1 - v2)
              }.sum
          }.sum
      }
    }
  }

  println(Solution.fourSumCount(
    Array(0, 1, -1),
    Array(-1, 1, 0),
    Array(0, 0, 1),
    Array(-1, 1, 1),
  ))

}
