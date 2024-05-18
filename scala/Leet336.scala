object Leet336 extends App {

  import scala.collection.mutable

  object Solution {
    def palindromePairs(words: Array[String]): List[List[Int]] = {
      var result = mutable.ListBuffer.empty[List[Int]]
      words
        .zipWithIndex
        .combinations(2)
        .foreach {
          case Array((w1, i1), (w2, i2)) =>
            if (isPalindrome(w1 + w2)) {
              result.append(List(i1, i2))
            }
            if (isPalindrome(w2 + w1)) {
              result.append(List(i2, i1))
            }
        }
      result.toList
    }

    def isPalindrome(word: String): Boolean = {
      var pointer1 = 0
      var pointer2 = word.length - 1
      while (pointer1 < pointer2) {
        if (word(pointer1) != word(pointer2)) {
          return false
        }
        pointer1 += 1
        pointer2 -= 1
      }
      true
    }
  }

}
