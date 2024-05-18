object Leet859 extends App {

  object Solution {
    def buddyStrings(s: String, goal: String): Boolean = {
      if (s.length != goal.length) {
        return false
      }
      val indicesOfDiffs = (0 until s.length).filter(i => s(i) != goal(i))
      (indicesOfDiffs.isEmpty && s.toSet.size != s.length) ||
        (indicesOfDiffs.length == 2 && indicesOfDiffs.map(s(_)) == indicesOfDiffs.map(goal(_)).reverse)
    }
  }


}
