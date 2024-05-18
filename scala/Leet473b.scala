object Leet473b extends App {

  object Solution {
    def combinationExist(remaining: Array[Int], sideLength: Int): Boolean = {
      if (remaining.isEmpty) return true
      // build iterator of all possible combinations
      (1 to remaining.length).foldLeft(Iterator.empty[Array[Int]]) {
          case (iterator, arraySize) => iterator ++ remaining.combinations(arraySize)
        }
        // find a solution
        .exists(used => used.sum == sideLength && combinationExist(remaining = remaining.diff(used), sideLength = sideLength))
    }

    def makesquare(matchsticks: Array[Int]): Boolean = {
      matchsticks.sum match {
        case totalLength if totalLength % 4 == 0 =>
          combinationExist(remaining = matchsticks, sideLength = totalLength / 4)
        case _ => false
      }
    }
  }

  println(Solution.makesquare(Array(1, 1, 2, 2, 2)))
  println(Solution.makesquare(Array(3, 3, 3, 3, 4)))
  println(Solution.makesquare(Array(5, 5, 5, 5, 4, 4, 4, 4, 3, 3, 3, 3)))
  println(Solution.makesquare(Array(13, 11, 1, 8, 6, 7, 8, 8, 6, 7, 8, 9, 8)))
  println(Solution.makesquare(Array(4, 13, 1, 1, 14, 15, 1, 3, 13, 1, 3, 5, 2, 8, 12)))

}
