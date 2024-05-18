object Leet291 extends App {

  object Solution {
    def wordPatternMatch(pattern: String, s: String): Boolean = {
      search(
        patterns = pattern.toList.distinct,
        substring = s,
        knownPatterns = Map(),
        targetPattern = pattern,
        target = s
      )
    }

    def search(patterns: List[Char], substring: String, knownPatterns: Map[Char, String], targetPattern: String, target: String): Boolean = {
      patterns match {
        case pattern :: restPatterns =>
          possibleSplits(substring).foldLeft(false) {
            case (found, (prefix, suffix)) if !found &&
              // check that the pattern have not been used yet
              !knownPatterns.exists(_._2 == prefix) => search(
              patterns = restPatterns,
              substring = suffix,
              knownPatterns = knownPatterns + (pattern -> prefix),
              targetPattern = targetPattern,
              target = target
            )
            // if found, propagate the result
            case (found, _) => found
          }
        case Nil => targetPattern.foldRight(List.empty[String]) {
          case (c, l) => knownPatterns(c) :: l
        }.mkString == target
      }
    }

    def possibleSplits(s: String): List[(String, String)] = {
      (1 to s.length).toList.map {
        l => s.splitAt(l)
      }
    }
  }

  println(Solution.wordPatternMatch("aba", "redblueredblue"))
  println(Solution.wordPatternMatch("aa", "redblueredblue"))
  println(Solution.wordPatternMatch("abba", "redblueredblue"))
  println(Solution.wordPatternMatch("abab", "redblueredblue"))
  println(Solution.wordPatternMatch("ab", "aa"))

}
