object Leet487 extends App {

  object Solution {
    def findMaxConsecutiveOnes(nums: Array[Int]): Int = {
      nums.zipWithIndex.foldLeft(List.empty[Int]) {
          case (startingPoints, (_, 0)) =>
            0 :: startingPoints
          case (startingPoints, (num, index)) if nums(index - 1) == 0 =>
            index :: startingPoints
          case (startingPoints, _) => startingPoints
        }
        .map(lengthFromIndex(_, nums))
        .max
    }

    def lengthFromIndex(index: Int, nums: Array[Int]): Int = {
      (index until nums.length).foldLeft(0, 1) {
        // allows to skip 0 limited number of times
        case ((sum, zeroesAllowed), index) if nums(index) == 0 && zeroesAllowed > 0 =>
          (sum + 1, zeroesAllowed - 1)
        // count ones
        case ((sum, zeroesAllowed), index) if nums(index) == 1 =>
          (sum + 1, zeroesAllowed)
        case ((sum, _), _) =>
          return sum
      }._1
    }
  }

}
