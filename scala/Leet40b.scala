object Leet40b extends App {

  import scala.collection.mutable

  object Solution {
    def combinationSum2(candidates: Array[Int], target: Int): List[List[Int]] = {
      val results = mutable.ListBuffer.empty[List[Int]]
      val availablePicks = candidates.groupBy(n => n).map { case (n, arr) => (n, arr.length) }.toList
      search(
        candidates = availablePicks,
        results = results,
        target = target
      )
      results.toList
    }

    private def search(
                        candidates: List[(Int, Int)],
                        results: mutable.ListBuffer[List[Int]],
                        target: Int,
                        currentSum: Int = 0,
                        path: List[Int] = List()
                      ) {
      if (currentSum == target) {
        results.append(path)
      } else if (currentSum < target) {
        candidates.headOption match {
          case Some((num, quantity)) =>
            // now we have 2 options: pick or skip
            //
            // to avoid duplicates we skip over all repeating characters
            // repeating characters will be checked during the pick option only
            // for example, if we have 1,1,1,1,2 we will jump to the 2
            // skip
            search(
              candidates = candidates.tail,
              results = results,
              target = target,
              currentSum = currentSum,
              path = path
            )
            // pick
            search(
              candidates = if (quantity > 1) {
                (num, quantity - 1) :: candidates.tail
              } else {
                candidates.tail
              },
              results = results,
              target = target,
              currentSum = currentSum + num,
              path = num :: path
            )
          case None =>
        }
      }
    }
  }
}
