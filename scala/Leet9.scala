object Leet9 extends App {

  object Solution {
    def isPalindrome(x: Int): Boolean = {
      x >= 0 && x == rev(x, scala.math.pow(10, len(x) - 1).toInt)
    }

    def len(x: Int): Int = {
      if (x < 10)
        1
      else
        len(x / 10) + 1
    }

    def rev(x: Int, m: Int): Int = {
      if (x < 10)
        x
      else
        x % 10 * m + rev((x - x % 10) / 10, m / 10)
    }
  }

  println(123456 % 10)
  println(Solution.rev(123456, 100000))
  println(Solution.isPalindrome(123456))
  println(Solution.isPalindrome(1221))
  println(Solution.isPalindrome(11211))
  println(Solution.isPalindrome(0))
  println(Solution.rev(0, 0))


}
