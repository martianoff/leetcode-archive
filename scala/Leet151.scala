object Leet151 extends App {

  object Solution {
    def reverseWords(s: String): String = {
      s.split(" ").filter {
        _.nonEmpty
      }.reverse.mkString(" ")
    }
  }

  println(Solution.reverseWords("a good   example"))

}
