object Leet64 extends App {

  import scala.util.chaining._

  object Solution {
    def minPathSum(grid: Array[Array[Int]]): Int = {
      // init O(M) DP array with -1 for simplicity
      grid.foldLeft(Array.fill(grid(0).length)(-1)) {
        case (minRow, sourceRow) =>
          // aggegate min path for each row in minRow
          minRow.zipWithIndex.foldLeft(Array.fill(minRow.length)(0)) {
            // for the top most sum of left + current
            case (curRow, (topValue, index)) if topValue == -1 => curRow.tap {
              r => r(index) = sourceRow(index) + leftValue(r, index).getOrElse(0)
            }
            // for others min(sum of top + current, sum of left + current)
            case (curRow, (topValue, index)) => curRow.tap {
              r =>
                r(index) = (leftValue(r, index) match {
                  case None => (sourceRow(index) + topValue)
                  case Some(l) => (sourceRow(index) + topValue) min (sourceRow(index) + l)
                })
            }
          }
      }.last
    }

    def leftValue(arr: Array[Int], index: Int): Option[Int] = if (index == 0) {
      None
    } else {
      Some(arr(index - 1))
    }
  }

}
