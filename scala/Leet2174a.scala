object Leet2174a extends App {

  import scala.collection.mutable

  object Solution {
    def removeOnes(grid: Array[Array[Int]]): Int = {
      val queue = mutable.Queue.empty[(Int, Int)]
      var m = Option.empty[Int]
      // search for possible starting points
      grid.zipWithIndex.foreach {
        case (row, rowId) =>
          row.zipWithIndex.foreach {
            case (v, colId) if v == 1 =>
              queue.enqueue((rowId, colId))
            case _ =>
          }
      }
      // traverse all starting points and search the best option
      while (queue.nonEmpty) {
        queue.dequeue() match {
          case (rowIdToRemove, colIdToRemove) =>
            // cleanup and search
            m = (removeOnes(
              grid.zipWithIndex.foldRight(List.empty[Array[Int]]) {
                case ((row, rowId), list) if rowId == rowIdToRemove => row.map(_ => 0) :: list
                case ((row, rowId), list) => row.zipWithIndex.map {
                  case (_, colId) if colId == colIdToRemove => 0
                  case (v, colId) => v
                } :: list
              }.toArray
            ) + 1) match {
              case minFlips => m match {
                case Some(oldMin) => Some(oldMin min minFlips)
                case _ => Some(minFlips)
              }
            }
        }
      }
      m.getOrElse(0)
    }
  }

  println(Solution.removeOnes(Array(Array(0, 1, 0), Array(1, 0, 1), Array(0, 1, 0))))
}
