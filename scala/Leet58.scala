object Leet58 extends App {

  object Solution {
    def lengthOfLastWord(s: String): Int = {
      lengthOfLastTrimmedWord(s.trim)
    }

    def lengthOfLastTrimmedWord(s: String): Int = {
      s.length - (s.lastIndexOf(" ") + 1)
    }
  }

  println(Solution.lengthOfLastWord(""))
  println(Solution.lengthOfLastWord("ss"))
  println(Solution.lengthOfLastWord("ss  "))
  println(Solution.lengthOfLastWord("a asd wqei eie"))

}
