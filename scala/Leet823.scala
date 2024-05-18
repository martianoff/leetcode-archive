object Leet823 extends App {

  import scala.collection.immutable
  import scala.collection.mutable
  import scala.util.chaining._

  object Solution {
    def numFactoredBinaryTrees(arr: Array[Int]): Int = {
      val cache = mutable.Map.empty[Int, Long]
      arr
        // find possible binary trees for a specific tree root
        .map(numFactoredBinaryTreesForRoot(immutable.TreeSet.from(arr), _, cache))
        .sum
        // return modulo 10^9 + 7 by task request
        .pipe {
          v =>
            (v % 1000000007L).toInt
        }
    }

    def numFactoredBinaryTreesForRoot(options: immutable.TreeSet[Int],
                                      root: Int,
                                      cache: mutable.Map[Int, Long]): Long = {
      cache.get(root) match {
        case Some(cached) => cached
        case None =>
          (
            1 +
              // attempt to use different left values
              // left value can from 2 to root/2
              options.range(from = 2, until = root / 2 + 1).toList.map {
                  case left if root % left == 0 && options.contains(root / left) =>
                    // calculate tree count for the left node
                    numFactoredBinaryTreesForRoot(options = options, root = left, cache = cache) *
                      // calculate tree count for the right node
                      numFactoredBinaryTreesForRoot(options = options, root = root / left, cache = cache)
                  case _ => 0
                }
                .sum
            )
            // save into cache
            .tap {
              v =>
                cache += (root -> v)
            }
      }
    }
  }

  //3
  println(Solution.numFactoredBinaryTrees(Array(2, 4)))
  //4
  println(Solution.numFactoredBinaryTrees(Array(2, 4, 5)))
  //7
  println(Solution.numFactoredBinaryTrees(Array(2, 4, 5, 10)))

}
