object Leet47 extends App {
  object Solution {
    def permuteUnique(nums: Array[Int]): List[List[Int]] = {
      permute(nums.zipWithIndex.map { case (v, i) => (i, v) }.toMap).distinct
    }

    def permute(nums: Map[Int, Int]): List[List[Int]] = {
      nums.size match {
        case 0 => List.empty[List[Int]]
        case 1 => List(List(nums.head._2))
        case _ => nums.flatMap {
          case (i, v) => permute(nums - i).map(v :: _)
        }.toList.distinct
      }
    }
  }

  println(Solution.permuteUnique(nums = Array(1, 1, 2)))
}
