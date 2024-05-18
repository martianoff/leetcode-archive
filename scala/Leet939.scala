object Leet939 extends App {

  object Solution {
    def minAreaRect(points: Array[Array[Int]]): Int = {
      // convert points to Map(Y -> Set(X1,X2...))
      val yxMap = points.map {
        case Array(x, y) => (y, x)
      }.groupBy(_._1).map {
        case (y, pairs) => (y, pairs.map(_._2).toSet)
      }
      // search for min
      points.foldLeft(Option.empty[Int]) {
        case (m, Array(x1, y1)) =>
          (m,
            points.foldLeft(Option.empty[Int]) {
              // search for left bottom and top right corners
              case (minLine, Array(x2, y2)) if x2 > x1 && y2 > y1
                // check if both other corners exist
                && yxMap(y1).contains(x2)
                && yxMap(y2).contains(x1) =>
                (minLine, (y2 - y1).abs * (x2 - x1).abs) match {
                  case (Some(oldMin), newValue) => Some(oldMin min newValue)
                  case (None, newValue) => Some(newValue)
                  case _ => minLine
                }
              case (minLine, _) => minLine
            }) match {
            case (Some(oldMin), Some(newValue)) => Some(oldMin min newValue)
            case (None, Some(newValue)) => Some(newValue)
            case _ => m
          }
        case (m, _) => m
      }.getOrElse(0)
    }
  }

  /**
   * [
   * [1,1],
   * [1,3],
   * [3,1],
   * [3,3],
   * [2,2]
   * ]
   */
  println(Solution.minAreaRect(Array(Array(1, 1), Array(1, 3), Array(3, 1), Array(3, 3), Array(2, 2))))
}
