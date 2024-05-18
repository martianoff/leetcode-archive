import scala.collection.immutable.TreeSet

object Leet583 extends App {

  import scala.util.chaining._

  object Solution {
    def minDistance(word1: String, word2: String): Int = {
      word1.length + word2.length - 2 * longestCommon(word1, word2)
    }

    private def longestCommon(word1: String, word2: String): Int = {
      val dp = Array.fill(word1.length + 1)(Array.fill(word2.length + 1)(0))
      (1 to word1.length).foreach {
        char1Index =>
          (1 to word2.length).foreach {
            char2Index =>
              dp(char1Index)(char2Index) = (
                if (word1(char1Index - 1) == word2(char2Index - 1)) {
                  dp(char1Index - 1)(char2Index - 1) + 1
                } else {
                  dp(char1Index - 1)(char2Index) max dp(char1Index)(char2Index - 1)
                }
                )
          }
      }
      dp
        //.tap(a => a.map(l => println(l.mkString(","))))
        .pipe(a => a(a.length - 1)(a(0).length - 1))
    }
  }

  assert(Solution.minDistance("leetcode", "etco") == 4)

}
