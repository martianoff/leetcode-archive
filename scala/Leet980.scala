object Leet980 extends App {

  object Solution {
    private val directions = Seq((-1, 0), (1, 0), (0, -1), (0, 1))

    def uniquePathsIII(grid: Array[Array[Int]]): Int = {
      val startOpt = find(grid, 1)
      println(startOpt)
      startOpt match {
        case Some(start) =>
          // use vector as immutable array to preserve state
          solve(
            grid = Vector.from(grid.map(Vector.from(_))),
            start = start,
            shouldVisit = countEmpty(grid))
        case _ => -1
      }
    }

    def solve(grid: Vector[Vector[Int]], start: (Int, Int), shouldVisit: Int): Int = {
      directions.map {
        // check borders and cell status
        case (yOff, xOff) if start._1 + yOff >= 0 &&
          start._1 + yOff <= grid.size - 1 &&
          start._2 + xOff >= 0 &&
          start._2 + xOff <= grid(0).size - 1 &&
          grid(start._1 + yOff)(start._2 + xOff) != -1 &&
          grid(start._1 + yOff)(start._2 + xOff) != 1 =>
          if (grid(start._1 + yOff)(start._2 + xOff) == 0) {
            solve(
              // update state
              grid = grid
                .updated(start._1 + yOff, grid(start._1 + yOff)
                  .updated(start._2 + xOff, -1)),
              start = ((start._1 + yOff), (start._2 + xOff)),
              shouldVisit = shouldVisit - 1)
          } else if (shouldVisit == 0) {
            1
          } else {
            0
          }
        case _ => 0
      }.sum
    }

    private def find(grid: Array[Array[Int]], value: Int): Option[(Int, Int)] = {
      grid.zipWithIndex.foreach {
        case (row, y) => row.zipWithIndex.foreach {
          case (v, x) if v == value => return Some((y, x))
          case _ =>
        }
      }
      None
    }

    private def countEmpty(grid: Array[Array[Int]]): Int = {
      grid.map {
        row =>
          row.map {
            case v if v == 0 => 1
            case _ => 0
          }.sum
      }.sum
    }
  }

}
