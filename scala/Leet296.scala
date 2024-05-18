import scala.collection.mutable

object Leet296 extends App {

  object Solution {
    def minTotalDistance(grid: Array[Array[Int]]): Int = {
      val height = grid.length
      val width = grid(0).length
      // get all X coordinates in sorted order
      val allXpoints = (0 until width).flatMap { x =>
        (0 until height).foldLeft(List.empty[Int]) {
          case (l, y) if grid(y)(x) == 1 =>
            x :: l
          case (l, _) => l
        }.reverse
      }.toArray
      // get all Y coordinates in sorted order
      val allYpoints = grid.zipWithIndex.flatMap {
        case (row, y) =>
          row.zipWithIndex.filter(_._1 == 1).map(_ => y)
      }
      minDistanceToPos(allXpoints, allXpoints(allXpoints.length / 2)) +
        minDistanceToPos(allYpoints, allYpoints(allYpoints.length / 2))
    }

    private def minDistanceToPos(points: Array[Int], pos: Int) = {
      points.map(point => (point - pos).abs).sum
    }
  }

}
