object Leet90 extends App {
  object Solution {
    def subsetsWithDup(nums: Array[Int]): List[List[Int]] = {
      subsets(nums.sorted.toList).distinct
    }

    def subsets(nums: List[Int]): List[List[Int]] = {
      nums match {
        case Nil => List(List.empty[Int])
        case h :: t =>
          subsets(t) match {
            case s =>
              // subsets with number
              s.map(h :: _) ++
                // subsets without number
                s
          }
      }
    }
  }
}
