import scala.annotation.tailrec

object Leet179 extends App {

  case class TrieNode(
                       private val nodes: collection.mutable.Map[Int, TrieNode],
                       private var nums: List[Int]
                     ) {
    def hasKey(k: Int): Boolean = {
      nodes.contains(k)
    }

    def addNumber(num: Int): Unit = {
      addSeq(num.toString.toSeq, num)
    }

    def getMax: String = {
      var result = ""
      // concat all numbers from node itself
      val current = nums.map(_.toString).foldLeft("") {
        _ ++ _
      }
      // get child nodes maximums
      val nodesMax = collection.mutable.Map.empty[Int, String]
      for (i <- 9 to 0 by -1) {
        if (hasKey(i)) {
          nodesMax(i) = nodes(i).getMax
        }
      }
      // prepend bigger numbers to the left
      for (i <- 9 to 0 by -1) {
        if (nodesMax.contains(i)) {
          val r = nodesMax(i)
          // here we are not sure if it is better to prepend or append numbers from child nodes
          // so we have to check what option is better, see use case [34323,3432]
          if (BigInt(r + current) >= BigInt(current + r))
            result += r
        }
      }
      // prepend number from node itself
      result += current
      // append smaller numbers to the right
      for (i <- 9 to 0 by -1) {
        if (nodesMax.contains(i)) {
          val r = nodesMax(i)
          if (BigInt(r + current) < BigInt(current + r))
            result += r
        }
      }
      result
    }

    // build trie with numbers
    private def addSeq(num: Seq[Char], original: Int): Unit = {
      num match {
        case Seq(first, tail@_*) =>
          val digit = first.toString.toInt
          if (!hasKey(digit)) {
            nodes(digit) = TrieNode(
              nodes = collection.mutable.Map(),
              nums = List()
            )
          }
          nodes(digit).addSeq(tail, original)
        case Seq() => nums = original :: nums
      }
    }
  }

  //O(n) time, O(n) space
  object Solution {
    def largestNumber(nums: Array[Int]): String = {
      val trie =
        TrieNode(nodes = collection.mutable.Map(), nums = List())
      nums.foreach(trie.addNumber)
      BigInt(trie.getMax).toString()
    }
  }


  println(Solution.largestNumber(Array(3, 30, 34, 5, 9))) //9534330
  println(Solution.largestNumber(Array(95, 9, 6))) //9956
  println(Solution.largestNumber(Array(34323, 3432))) //343234323
  println(Solution.largestNumber(Array(999999998, 999999997, 999999999))) //
  println(Solution.largestNumber(Array(0, 0))) //

}
