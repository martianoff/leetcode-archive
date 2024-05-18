object Leet926a extends App {

  object Solution {
    def minFlipsMonoIncr(s: String): Int = {
      // calc cost of flipping all to ones
      val allOnesCost =
        (0 until s.length).collect {
          case i if s(i) == '0' => 1
        }.sum
      // shift window to the right and recalculate cost
      (0 until s.length).foldLeft((allOnesCost, allOnesCost)) {
        case ((prevCost, minCost), onesPos) =>
          val newCost =
            if (s(onesPos) == '1') {
              prevCost + 1
            } else {
              prevCost - 1
            }
          (
            newCost,
            minCost min newCost
          )
      }._2
    }
  }

}
