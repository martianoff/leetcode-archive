object Leet78 extends App {
  object Solution {
    def subsets(nums: Array[Int]): List[List[Int]] = {
      subset(nums.toList)
    }

    private def subset(nums: List[Int]): List[List[Int]] = {
      nums match {
        case List() => List.empty[List[Int]]
        // for the last element answer is an element or empty
        case List(n) => List(List(n), List())
        case n :: tail =>
          subset(tail) match {
            case subsetsFromTail =>
              // subsets without the number
              subsetsFromTail ++
                // subsets with the number
                subsetsFromTail.map(n :: _)
          }
      }
    }
  }
}
