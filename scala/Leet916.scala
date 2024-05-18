object Leet916 extends App {
  object Solution {

    import scala.util.chaining._

    object Solution {
      def wordSubsets(words1: Array[String], words2: Array[String]): List[String] = {
        val words2map = words2.map(freqMap)
        words1.map(w => (w, freqMap(w))).map {
          case (w, word1) =>
            words2map
              .map(word2 => universal(word1, word2))
              .reduceLeft(_ && _)
              .pipe {
                case true => Some(w)
                case false => None
              }
        }.collect {
          case Some(w) => w
        }.toList
      }

      def universal(word1: Map[Char, Int], word2: Map[Char, Int]): Boolean = {
        word2.foreach {
          case (char, count) if word1.get(char).getOrElse(0) < count => return false
          case _ =>
        }
        return true
      }

      def freqMap(word: String): Map[Char, Int] = {
        word.groupBy(c => c).map { case (c, arr) => (c, arr.length) }
      }
    }
  }

}
