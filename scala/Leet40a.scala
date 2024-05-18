object Leet40a extends App {

  import scala.collection.mutable

  object Solution {
    def combinationSum2(candidates: Array[Int], target: Int): List[List[Int]] = {
      val results = mutable.ListBuffer.empty[Map[Int, Int]]
      search(
        candidates = candidates,
        results = results,
        target = target,
      )
      results
        // filter duplicates
        .distinct
        // unpack maps
        .map(freqMap => freqMap.foldLeft(List.empty[Int]) {
          case (aggr, (num, quantity)) =>
            aggr ++ (0 until quantity).foldLeft(List.empty[Int]) {
              case (aggr, _) => num :: aggr
            }
        })
        .toList
    }

    private def search(
                        candidates: Array[Int],
                        results: mutable.ListBuffer[Map[Int, Int]],
                        target: Int,
                        index: Int = 0,
                        currentSum: Int = 0,
                        path: Map[Int, Int] = Map.empty[Int, Int]
                      ) {
      if (currentSum == target) {
        results.append(path)
      } else if (index < candidates.length && currentSum < target) {
        // pick
        search(
          candidates = candidates,
          results = results,
          target = target,
          index = index + 1,
          currentSum = currentSum + candidates(index),
          // pack path as a map
          path = path.get(candidates(index)) match {
            case Some(v) => path + (candidates(index) -> (v + 1))
            case None => path + (candidates(index) -> 1)
          }
        )
        // skip
        search(
          candidates = candidates,
          results = results,
          target = target,
          index = index + 1,
          currentSum = currentSum,
          path = path
        )
      }
    }
  }
}
