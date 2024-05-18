object Leet996 extends App {

  import scala.math._

  object Solution {
    def numSquarefulPerms(nums: Array[Int]): Int = {
      nums.permutations.count(isSquarefulArray)
    }

    private def isSquarefulArray(array: Array[Int]): Boolean = {
      array.sliding(2, 1).foldLeft(true) {
        case (squareful, Array(a, b)) if isPerfectSquare(a + b) => squareful
        case _ => return false
      }
    }

    private def isPerfectSquare(num: Int) = {
      sqrt(num) % 1 == 0
    }
  }

  //[89,72,71,44,50,72,26,79,33,27,84]
}
