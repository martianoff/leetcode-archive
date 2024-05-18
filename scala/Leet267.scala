object Leet267 extends App {

  object Solution {
    def generatePalindromes(s: String): List[String] = {
      permute(
        s.foldLeft(Map.empty[Char, Int]) {
          case (map, char) => map.get(char) match {
            case Some(q) => map + (char -> (q + 1))
            case None => map + (char -> 1)
          }
        }
      )
        .filter(isPalindrome)
        .map(_.mkString)
    }

    def permute(m: Map[Char, Int]): List[List[Char]] = {
      if (m.isEmpty) return List(List.empty[Char])
      m.foldLeft(List.empty[List[Char]]) {
        case (aggr, (k, v)) if v > 0 => permute(m.get(k) match {
          // remove empty keys
          case Some(v) if v == 1 => m - k
          // decrement usage
          case Some(v) if v > 1 => m + (k -> (v - 1))
          case _ => Map.empty[Char, Int]
        })
          .map(k :: _) ++ aggr
        case (aggr, _) => aggr
      }
    }

    def isPalindrome(s: List[Char]): Boolean = {
      s == s.reverse
    }
  }

  println(Solution.generatePalindromes("a"))
  println(Solution.generatePalindromes("abc"))
  println(Solution.generatePalindromes("aabbcc"))
  println(Solution.generatePalindromes("aabbhijkkjih"))

}
