object Leet931 extends App {

  object Solution {
    //O(N*M) time, O(M) space
    def minFallingPathSum(matrix: Array[Array[Int]]): Int = {
      matrix.zipWithIndex.foldLeft(List[Int]()) {
        case (_, (row, rowId)) if rowId == 0 => row.toList
        case (prevRow, (row, _)) =>
          // best options for row
          row.zipWithIndex.map {
            case (v, colId) =>
              v + Seq(
                // left
                if (colId > 0) {
                  Some(prevRow(colId - 1))
                } else None,
                // top
                Some(prevRow(colId)),
                // right
                if (colId < row.length - 1) {
                  Some(prevRow(colId + 1))
                } else None
              ).collect {
                case Some(n) => n
              }.min
          }.toList
      }.min
    }
  }

  /**
   * [
   * [2,1,3],
   * [6,5,4],
   * [7,8,9]
   * ]
   */
  //=13
  println(Solution.minFallingPathSum(Array(Array(2, 1, 3), Array(6, 5, 4), Array(7, 8, 9))))
}
