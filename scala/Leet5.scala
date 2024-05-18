object Leet5 extends App {

  object Solution {
    def longestPalindrome(s: String): String = {
      var result = ""
      // trying different center of palindrome
      (0 until s.length).foreach { center =>
        // centerLength 1 - odd length (XXAXX), expand from single center to both sides
        // centerLength 2 - even length (XXXX), expand from double center to both sides
        (1 to 2).foreach {
          centerLength =>
            var left = center
            var right = center + centerLength - 1
            while (left >= 0 && right < s.length && s(left) == s(right)) {
              if (right - left + 1 > result.length) {
                result = s.slice(left, right + 1)
              }
              left -= 1
              right += 1
            }
        }
      }
      result
    }
  }

}
