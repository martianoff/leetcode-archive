object Leet2174 extends App {

  import scala.collection.immutable

  object Solution {

    def dfs(queue: immutable.Queue[(Int, Int)], grid: Array[Array[Int]], globalMin: Option[Int]): Int =
      if (queue.nonEmpty) {
        queue.dequeue match {
          case ((rowIdToRemove, colIdToRemove), restQueue) =>
            // cleanup grid and search
            (removeOnes(
              grid.zipWithIndex.foldRight(List.empty[Array[Int]]) {
                case ((row, rowId), list) if rowId == rowIdToRemove => row.map(_ => 0) :: list
                case ((row, _), list) => row.zipWithIndex.map {
                  case (_, colId) if colId == colIdToRemove => 0
                  case (v, _) => v
                } :: list
              }.toArray
            ) + 1 match {
              // pick between Option[Int] to Int
              case minFlips => globalMin match {
                case Some(oldMin) => oldMin min minFlips
                case _ => minFlips
              }
            }) match {
              case bestResultForThisStartingPoint =>
                bestResultForThisStartingPoint min
                  // get best results for other starting points
                  dfs(queue = restQueue, grid = grid, globalMin = Some(bestResultForThisStartingPoint))
            }
        }
      } else globalMin.getOrElse(0)

    def removeOnes(grid: Array[Array[Int]]): Int = {
      dfs(
        // get all points with "1" and push them to immutable.Queue
        queue = immutable.Queue.from(
          grid.zipWithIndex.foldLeft(List.empty[(Int, Int)]) {
            case (list, (row, rowId)) =>
              list ++ row.zipWithIndex.collect {
                case (v, colId) if v == 1 =>
                  (rowId, colId)
              }.toList
          }
        ),
        grid = grid,
        globalMin = None)
    }
  }

  //2
  println(Solution.removeOnes(Array(Array(0, 1, 0), Array(1, 0, 1), Array(0, 1, 0))))
  //2
  println(Solution.removeOnes(Array(Array(1, 1, 1, 0), Array(1, 0, 1, 1), Array(0, 1, 1, 0))))

}
