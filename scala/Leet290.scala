object Leet290 extends App {

  import scala.collection.mutable
  import scala.util.chaining._

  object Solution {
    def wordPattern(pattern: String, s: String): Boolean = {
      val patterns = mutable.Map.empty[Char, String]
      val assigned = mutable.Set.empty[String]
      s.split(' ').zipWithIndex.tap {
        list =>
          if (list.length != pattern.length) {
            return false
          }
      }.foreach {
        case (word, index) if !assigned.contains(word) && !patterns.contains(pattern(index)) =>
          assigned += word
          patterns(pattern(index)) = word
        case (word, index) if assigned.contains(word) && !patterns.contains(pattern(index)) =>
          return false
        case (word, index) if patterns(pattern(index)) != word =>
          return false
        case _ =>
      }
      true
    }
  }

}
