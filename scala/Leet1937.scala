object Leet1937 extends App {

  object Solution {
    def maxPoints(points: Array[Array[Int]]): Long = {
      // row by row calculate base possible placement from the previous line
      points.zipWithIndex.foldLeft(Array.empty[Long]) {
        // line one as is
        case (_, (currentRow, lineIndex)) if lineIndex == 0 => currentRow.map(_.toLong)
        case (prevRow, (currentRow, _)) =>
          // max possible points from the left side on prev row
          val l = prevRow.foldLeft(List.empty[Long]) {
            case (newRow, value) =>
              newRow.headOption match {
                // maximum so far (either this one or last one minus cost)
                case Some(prevValue) => value.max(prevValue - 1) :: newRow
                // first number as is
                case None => value :: newRow
              }
          }.toArray
          // max possible points from the right side on prev row
          val r = prevRow.foldRight(List.empty[Long]) {
            case (value, newRow) =>
              newRow.headOption match {
                // maximum so far (either this one or last one minus cost)
                case Some(prevValue) => value.max(prevValue - 1) :: newRow
                // first number as is
                case None => value :: newRow
              }
          }.toArray
          // current maximum using left and right arrays
          currentRow.zipWithIndex.map {
            case (v, i) =>
              v + (l(currentRow.length - 1 - i) max r(i))
          }
      }.max
    }
  }

  /**
   * [[0,0,4,1,4],
   * [2,1,2,0,1],
   * [2,2,1,3,4],
   * [5,2,4,5,4],
   * [0,5,4,2,5]]
   */
  println(Solution.maxPoints(Array(Array(0, 0, 4, 1, 4), Array(2, 1, 2, 0, 1), Array(2, 2, 1, 3, 4))))


  /**
   * [
   * [1,2,3],
   * [1,5,1],
   * [3,1,1]
   * ]
   */
  //=9
  println(Solution.maxPoints(Array(Array(1, 2, 3), Array(1, 5, 1), Array(3, 1, 1))))

  /**
   * [
   * [5,3],
   * [3,5],
   * [3,1]
   * ]
   */
  //=11
  println(Solution.maxPoints(Array(Array(5, 3), Array(3, 5), Array(3, 1))))

  /**
   * [
   * [1,5],
   * [3,2],
   * [4,2]
   * ]
   */
  //=11
  println(Solution.maxPoints(Array(Array(1, 5), Array(3, 2), Array(4, 2))))
}
