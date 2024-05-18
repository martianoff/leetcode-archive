object Leet336b extends App {

  import scala.collection.mutable
  import scala.util.chaining._

  object Solution {
    def palindromePairs(words: Array[String]): List[List[Int]] = {
      // build a trie
      val trie = new Trie(words)
      // search for the result
      words
        .zipWithIndex
        .map {
          case (word, index) => trie.searchForRemainder(word, index) match {
            case remainderIndices if remainderIndices.nonEmpty =>
              Some(remainderIndices.map {
                remainderIndex => List(index, remainderIndex)
              })
            case _ => None
          }
        }
        .collect {
          case Some(l) => l
        }.flatten.toList
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

    class Trie(words: Array[String]) {
      val root = new Node()
      var emptyIndex = Option.empty[Int]
      // import all words
      words
        .zipWithIndex
        .foreach {
          case (word, index) => importWordReversed(word, index)
        }

      def importWordReversed(word: String, index: Int): Unit = {
        if (word == "") {
          emptyIndex = Some(index)
        }
        word.reverse.foldLeft(root) {
          case (node, c) =>
            node.childNodes.get(c) match {
              case Some(n) => n
              case None => new Node().tap {
                n => node.childNodes(c) = n
              }
            }
        }.tap {
          lastNode => lastNode.index = Some(index)
        }
      }

      def searchForRemainder(word: String, index: Int): Set[Int] = {
        // case 1 (search the same word but reversed: abc+cbd)
        case1(word, index).toSet ++
          // case 2 (remainder is longer: sa+llas, ll must be palindrome and last part match to the reversed original world)
          case2(word, index).toSet ++
          // case 3 (remainder is shorter: abb+ba, bb must be palindrome and first part match to the reversed original world)
          case3(word, index).toSet
      }

      private def case1(word: String, index: Int): List[Int] = {
        word.foldLeft(root) {
          case (node, c) if !node.childNodes.contains(c) => return List()
          case (node, c) => node.childNodes(c)
        }.pipe {
          node =>
            node.index match {
              // don't reuse the same word
              case Some(i) if i != index => List(i)
              case _ => List()
            }
        }
      }

      private def case2(word: String, index: Int): List[Int] = {
        word.foldLeft(root, true) {
          case ((node, found), _) if !found => (node, false)
          case ((node, _), c) if !node.childNodes.contains(c) => (node, false)
          case ((node, found), c) => (node.childNodes(c), found)
        }.pipe {
          case (node, found) if found =>
            // search for palindromes
            var wordIndexes = mutable.ListBuffer.empty[Int]
            val queue = mutable.Queue.empty[Node]
            queue.enqueue(node)
            while (queue.nonEmpty) {
              queue.dequeue() match {
                case node =>
                  node.childNodes.foreach {
                    case (c, node) =>
                      node.index match {
                        case Some(i) => wordIndexes.append(i)
                        case _ =>
                      }
                      queue.enqueue(node)
                  }
              }
            }
            wordIndexes.collect { case i if isPalindrome(words(i).dropRight(word.length)) => i }.toList
          case _ => List()
        }
      }

      private def case3(word: String, index: Int): List[Int] = {
        var wordIndexes = mutable.ListBuffer.empty[Int]
        word.foldLeft(root, true) {
          case ((node, found), _) if !found => (node, false)
          case ((node, _), c) if !node.childNodes.contains(c) => (node, false)
          case ((node, found), c) =>
            (node.childNodes(c).tap {
              node =>
                node.index match {
                  case Some(i) if i != index => wordIndexes.append(i)
                  case _ =>
                }
            }, found)
        }.pipe {
          case (node, found) =>
            // search for palindromes in the first word
            emptyIndex match {
              case Some(i) if i != index => wordIndexes.append(i)
              case _ =>
            }
            wordIndexes.collect { case i if isPalindrome(word.drop(words(i).length)) => i }.toList
        }
      }

      class Node() {
        val childNodes = mutable.Map.empty[Char, Node]
        var index: Option[Int] = None
      }
    }
  }

}
