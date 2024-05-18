object Leet28 extends App {

  import scala.util.chaining._
  import scala.annotation.tailrec

  object Solution {
    def strStr(haystack: String, needle: String): Int = {
      needle.toList.pipe {
        needleList =>
          search(
            haystack = haystack.toList,
            restNeedle = needleList,
            needle = needleList
          )
      }
    }

    @tailrec
    private def search(haystack: List[Char], restNeedle: List[Char], needle: List[Char], matchedNeedle: List[Char] = List(), index: Int = 0): Int = {
      (haystack, restNeedle) match {
        case (_, Nil) => index - needle.length
        case (Nil, _) => -1
        case (char :: rest, nchar :: nrest) if char == nchar => search(
          haystack = rest,
          restNeedle = nrest,
          matchedNeedle = nchar :: matchedNeedle,
          needle = needle,
          index = index + 1
        )
        case (char :: rest, _) =>
          val newHaystack = matchedNeedle.reverse match {
            case (_ :: matched) =>
              // go back
              matched ++ (char :: rest)
            case Nil =>
              // continue from next char
              rest
          }
          search(
            haystack = newHaystack,
            restNeedle = needle,
            needle = needle,
            matchedNeedle = List(),
            index = index + haystack.length - newHaystack.length
          )

      }
    }
  }

  println(Solution.strStr("mississippi", "issipi"))
  println(Solution.strStr("mississippi", "issip"))
  println(Solution.strStr("mississippi", "iss"))

}
