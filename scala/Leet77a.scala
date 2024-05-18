import scala.collection.immutable.TreeSet

object Leet77a extends App {
  object Solution {
    def combine(n: Int, k: Int): List[List[Int]] = {
      combineOptions(TreeSet.from(1 to n), k)
    }

    def combineOptions(options: TreeSet[Int], k: Int): List[List[Int]] = {
      if (k == 0) List(List())
      else {
        options.toList.flatMap {
          v => combineOptions(options.rangeFrom(v + 1), k - 1).map(v :: _)
        }
      }
    }
  }

  println(Solution.combine(13, 13))
  println(Solution.combine(4, 2))
  println(Solution.combine(4, 3))
  println(Solution.combine(4, 4))

}
