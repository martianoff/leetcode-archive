
object Leet1332 extends App {

  object Solution {
    def removePalindromeSub(s: String): Int = {
      s match {
        case Palindrome() => 1
        case AB() => 2
        case _ => 1
      }
    }
  }

  object Palindrome {
    def unapply(s: String): Boolean = {
      s == s.reverse
    }
  }

  object AB {
    def unapply(s: String): Boolean = {
      s.toSet == Set('a', 'b')
    }
  }

  println(Solution.removePalindromeSub("abab"))
  println(Solution.removePalindromeSub("aaa"))
  println(Solution.removePalindromeSub("bbb"))
  println(Solution.removePalindromeSub("ababa"))

}
