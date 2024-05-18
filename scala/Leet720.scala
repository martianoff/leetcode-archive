

object Leet720 extends App {


  import scala.collection.mutable
  import scala.util.chaining._

  object Solution {
    def longestWord(words: Array[String]): String = {
      val trie = new Trie()
      words.map {
        w => trie.add(w.toList)
      }
      trie.longest()
    }

    class Trie {
      val nodes = mutable.Map.empty[Char, Trie]
      var endOfWord: Boolean = false

      def add(word: List[Char]): Trie = {
        word match {
          case head :: tail =>
            (nodes.get(head) match {
              case Some(trieNode) =>
                trieNode
              case None =>
                new Trie().tap(nodes(head) = _)
            }).pipe(n => tail match {
              case Nil => n.endOfWord = true
              case _ => n.add(tail)
            })
          case _ =>
        }
        this
      }

      def longest(): String = {
        longestPath().mkString
      }

      private def longestPath(): List[Char] = {
        if (nodes.isEmpty) return List()
        nodes.collect {
          case (char, n) if n.endOfWord => char :: n.longestPath
        } match {
          case l if l.nonEmpty =>
            l.reduceLeft {
              (a, b) =>
                (a.length, b.length) match {
                  case (al, bl) if al == bl && a.mkString < b.mkString => a
                  case (al, bl) if al == bl => b
                  case (al, bl) if al > bl => a
                  case (_, _) => b
                }
            }
          case _ => List()
        }
      }
    }

  }

  println(Solution.longestWord(Array("a", "banana", "app", "appl", "ap", "apply", "apple")))
  println(Solution.longestWord(Array("yo", "ew", "fc", "zrc", "yodn", "fcm", "qm", "qmo", "fcmz", "z", "ewq", "yod", "ewqz", "y")))

}
