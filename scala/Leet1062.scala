object Leet1062 extends App {
  object Solution {
    def longestRepeatingSubstring(s: String): Int = {
      (s.length - 1 to 1 by -1).foreach {
        substrLen =>
          s.sliding(substrLen, 1).foldLeft(Set.empty[String]) {
            case (aggr, sliding) if aggr.contains(sliding) => return substrLen
            case (aggr, sliding) => aggr + sliding
          }
      }
      0
    }
  }
}
