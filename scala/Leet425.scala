object Leet425 extends App {

  import scala.collection.mutable

  object Solution {
    def wordSquares(words: Array[String]): List[List[String]] = {
      val trie = new Trie()
      words.foreach { w =>
        trie.addWord(w.toList)
      }
      val results = mutable.ListBuffer.empty[List[String]]
      dfs(trie, words.length, words.toSet, results)
      results.toList
    }

    def dfs(trie: Trie, wordsCount: Int, availableWords: Set[String], results: mutable.ListBuffer[List[String]], combination: List[String] = List()): Unit = {
      if (combination.size == availableWords.head.size) {
        // we build block from bottom to up
        results.append(combination.reverse)
        return
      }
      availableWords.toList.foreach { w =>
        // we are building square from bottom to up, due to scala List implementation
        val proposedCombination = w :: combination
        // build prefixes after appending a new word
        val prefixes = (0 until w.size).map {
          charIndex =>
            proposedCombination.toList.map {
              word => word(charIndex)
            }
        }.map(_.reverse)
        // validate if we can build words using all of those prefixes
        if (prefixes.map(prefix => trie.searchPrefix(prefix).size > 0).reduceLeft {
          _ && _
        }) {
          dfs(
            trie,
            wordsCount,
            availableWords,
            results,
            proposedCombination
          )
        }
      }
      /*
      "abaa"
      "aaab"
      "baaa"
      "aaba"
      */
    }

    class Trie() {
      private val storage = mutable.Map.empty[Char, Trie]
      private var endOfWord = false

      override def toString: String = {
        if (endOfWord) {
          "*"
        } else {
          ""
        } + storage.toString
      }

      def addWord(word: List[Char]): Unit = {
        word match {
          case char :: tail =>
            if (!storage.contains(char)) {
              storage(char) = new Trie
            }
            if (tail.nonEmpty) {
              storage(char).addWord(tail)
            } else {
              storage(char).endOfWord = true
            }
          case _ =>
        }
      }

      def searchPrefix(prefix: List[Char]): List[List[Char]] = {
        var trie = this
        val prefixQueue = mutable.Queue.from(prefix)
        val words = mutable.ListBuffer.empty[List[Char]]
        val reversePrefix = prefix.reverse
        // scroll to prefix
        while (prefixQueue.nonEmpty) {
          val char = prefixQueue.dequeue()
          if (!trie.storage.contains(char)) {
            return List()
          }
          trie = trie.storage(char)
        }
        // bfs
        val explore = mutable.Queue.empty[(Trie, List[Char])]
        explore.enqueue((trie, reversePrefix))
        while (explore.nonEmpty) {
          explore.dequeue() match {
            case (trie, word) =>
              if (trie.endOfWord) {
                words.append(word)
              }
              trie.storage.foreach {
                case (char, trie) => explore.enqueue((trie, char :: word))
              }
          }
        }
        words.map(_.reverse).toList
      }
    }
  }
}
