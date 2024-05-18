object Leet1197 extends App {

  import scala.collection.mutable
  import scala.util.chaining._

  object Solution {

    val cache = mutable.Map.empty[(Int, Int), Int]

    def minKnightMoves(x: Int, y: Int): Int = {
      dfs(x, y)
    }

    def dfs(x: Int, y: Int): Int = {
      // solution is symmetric
      (x.abs, y.abs).pipe {
        // use cache if available
        case (x, y) if cache.contains((x, y)) =>
          cache((x, y))
        case (x, y) if x + y == 0 =>
          0
        case (x, y) if x + y == 2 =>
          2
        case (x, y) => (1 + (dfs(x - 1, y - 2) min dfs(x - 2, y - 1))).tap {
          // store result into cache
          v => cache += (x, y) -> v
        }
      }
    }
  }

}
