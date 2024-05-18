object Leet28b extends App {

  object Solution {
    def strStr(haystack: String, needle: String): Int = {
      haystack.indexOf(needle)
    }
  }

  println(Solution.strStr("mississippi", "issipi"))
  println(Solution.strStr("mississippi", "issip"))
  println(Solution.strStr("mississippi", "iss"))

}
