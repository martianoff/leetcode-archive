

object Leet217 extends App {

  object Solution {
    def containsDuplicate(nums: Array[Int]): Boolean = {
      !Iterator.unfold((0, Set.empty[Int], false)) {
        case (_, _, true) => None
        case (index, aggregator, finished) if index < nums.length && !aggregator.contains(nums(index))
        => Some(true, (index + 1, aggregator + nums(index), finished))
        case (index, aggregator, _) if index < nums.length && aggregator.contains(nums(index))
        => Some(false, (index + 1, aggregator + nums(index), true))
        case (_, _, _)
        => None
      }.reduceLeft(_ && _)
    }
  }

}
