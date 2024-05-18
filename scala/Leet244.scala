object Leet244 extends App {

  class WordDistance(_wordsDict: Array[String]) {

    val storage: Map[String, Array[Int]] = _wordsDict.zipWithIndex.groupBy(_._1).map {
      case (word, arr) => (word, arr.map(_._2))
    }

    def shortest(word1: String, word2: String): Int = {
      storage(word1).foldLeft(Int.MaxValue) {
        case (hmin, index1) => hmin min storage(word2).foldLeft(Int.MaxValue) {
          case (lmin, index2) => lmin min (index1 - index2).abs
        }
      }
    }

  }

}
