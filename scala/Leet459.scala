object Leet459 extends App {

  object Solution {
    def repeatedSubstringPattern(s: String): Boolean = {
      (1 to s.length / 2).foreach {
        case patternLength if (s.length % patternLength) == 0 =>
          if (validatedPatternLength(s, patternLength)) {
            return true
          }
        // doesn't match, check other options
        case _ =>
      }
      false
    }

    def validatedPatternLength(s: String, len: Int): Boolean = {
      val patternStart = s.head
      val patternEnd = s.last
      if (s(s.length - len) != patternStart || s(len - 1) != patternEnd) {
        return false
      }
      val pattern = s.substring(0, len)
      s.replace(pattern, "") == ""
    }
  }

}
