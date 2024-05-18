object Leet926 extends App {

  object Solution {
    def minFlipsMonoIncr(s: String): Int = {
      (0 to s.length).map { onesStartPos =>
        flipsToMake(s, onesStartPos)
      }.min
    }

    private def flipsToMake(s: String, onesStartPos: Int) = {
      // turn ones into zeroes
      (0 until onesStartPos).map {
        case i if s(i) == '1' => 1
        case _ => 0
      }.sum +
        // turn zeroes into ones
        (onesStartPos until s.length).map {
          case i if s(i) == '0' => 1
          case _ => 0
        }.sum
    }
  }

}
