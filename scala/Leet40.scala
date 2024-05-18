object Leet40 extends App {
  object Solution {
    def combinationSum2(candidates: Array[Int], target: Int): List[List[Int]] = {
      val combinationIterator = (1 to candidates.length).foldLeft(Iterator.empty[Array[Int]]) {
        case (iterator, arrSize) => iterator ++ candidates.combinations(arrSize)
      }
      combinationIterator.filter(_.sum == target).map(_.toList).toList
    }
  }
}
