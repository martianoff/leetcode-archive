object Leet96 extends App {

  import scala.util.chaining._
  import scala.collection.mutable

  object Solution {
    val cache = mutable.Map.empty[Int, Int]

    def numTrees(n: Int): Int = {
      if (n <= 1) return 1
      cache.get(n) match {
        case Some(cachedValue) => cachedValue
        case _ =>
          // try different possible root
          (1 to n).map(
              i => numTrees(i - 1) * numTrees(n - i)
            )
            .sum
            .tap {
              v => cache += (n -> v)
            }
      }
    }
  }

  //5
  println(Solution.numTrees(3))

  //14
  println(Solution.numTrees(4))

  //42
  println(Solution.numTrees(5))
}
