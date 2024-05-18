object Leet28a extends App {

  object Solution {
    def strStr(haystack: String, needle: String): Int = {
      haystack.sliding(needle.length, 1).foldLeft(0) {
        case (index, window) if window == needle => return index
        case (index, _) => index + 1
      }
      -1
    }
  }

  println(Solution.strStr("mississippi", "issipi"))
  println(Solution.strStr("mississippi", "issip"))
  println(Solution.strStr("mississippi", "iss"))

}
