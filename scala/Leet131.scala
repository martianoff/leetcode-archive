object Leet131 extends App {
  object Solution {
    def partition(s: String): List[List[String]] = {
      partitionBuf(s.toList)
    }

    def partitionBuf(buf: List[Char]): List[List[String]] = {
      (1 to (buf.size min 16)).toList.foldLeft(List.empty[List[String]]) {
        case (aggr, n) => buf.splitAt(n) match {
          case (l1, l2) if isPalindrome(l1) =>
            (if (l2.isEmpty) {
              List(List(l1.mkString))
            } else {
              partitionBuf(l2).map(l1.mkString :: _)
            }) ++ aggr
          case _ => aggr
        }
      }
    }

    def isPalindrome(list: List[Char]): Boolean = {
      list == list.reverse
    }
  }

  println(Solution.partition("abc"))
  println(Solution.partition("aab"))
  println(Solution.partition("aaa"))
  println(Solution.partition("aa"))
  println(Solution.partition("aabbcc"))
}
