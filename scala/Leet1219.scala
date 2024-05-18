object Leet1219 extends App {

  import scala.util.chaining._

  object Solution {
    def getMaxGoldFromPoint(grid: Array[Array[Int]], point: (Int, Int)): Int = {
      point match {
        case (x, y) =>
          grid(y)(x).pipe {
            // explore possible paths
            _ + (List((x - 1, y), (x, y - 1), (x + 1, y), (x, y + 1)).map {
              // check that possible next point is within borders and is not empty
              case (nextX, nextY) if nextX >= 0 && nextY >= 0 && nextY < grid.length && nextX < grid(0).length && grid(nextY)(nextX) > 0 =>
                getMaxGoldFromPoint(grid.map(_.clone).tap(_(y)(x) = 0), (nextX, nextY))
              case _ => 0
            } match {
              case l if l.isEmpty => 0
              case l => l.max
            })
          }
      }
    }

    def getMaximumGold(grid: Array[Array[Int]]): Int = {
      // get starting points
      grid.zipWithIndex.foldLeft(List.empty[(Int, Int)]) {
        case (list, (line, y)) =>
          line.zipWithIndex.foldLeft(List.empty[Int]) {
            case (list, (gold, x)) if gold > 0 => x :: list
            case (list, _) => list
          }.map(x => (x, y)) ++ list
        case (list, _) => list
      }.map {
        p =>
          getMaxGoldFromPoint(grid.map(_.clone), p)
      } match {
        case l if l.isEmpty => 0
        case l => l.max
      }
    }
  }

  //24
  //println(Solution.getMaximumGold(Array(Array(0,6,0),Array(5,8,7),Array(0,9,0))))
  //129
  println(Solution.getMaximumGold(Array(Array(0, 0, 0, 0, 0, 0, 32, 0, 0, 20), Array(0, 0, 2, 0, 0, 0, 0, 40, 0, 32), Array(13, 20, 36, 0, 0, 0, 20, 0, 0, 0), Array(0, 31, 27, 0, 19, 0, 0, 25, 18, 0), Array(0, 0, 0, 0, 0, 0, 0, 0, 0, 0), Array(0, 0, 0, 0, 0, 0, 0, 18, 0, 6), Array(0, 0, 0, 25, 0, 0, 0, 0, 0, 0), Array(0, 0, 0, 21, 0, 30, 0, 0, 0, 0), Array(19, 10, 0, 0, 34, 0, 2, 0, 0, 27), Array(0, 0, 0, 0, 0, 34, 0, 0, 0, 0))))

  /**
   * starting point: (2,1)
   * (2,1) -> (2,2)
   * (2,2) -> (1,2)
   * (1,2) -> (0,2)
   * (1,2) -> (1,3)
   * (1,3) -> (2,3)
   * (2,2) -> (2,3)
   */
}
