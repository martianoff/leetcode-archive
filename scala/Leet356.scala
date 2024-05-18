object Leet356 extends App {

  object Solution {
    def isReflected(points: Array[Array[Int]]): Boolean = {
      val aggrByY = points.foldLeft(Map.empty[Int, Set[Int]].withDefaultValue(Set())) {
        case (aggr, Array(x, y)) =>
          aggr + (y -> (aggr(y) + x))
      }
      val reflectionX = aggrByY.headOption match {
        case Some((_, xvalues)) => (xvalues.max + xvalues.min).toDouble / 2
        case _ => return false
      }
      aggrByY.foreach {
        case (_, xvalues) =>
          xvalues.foreach {
            case x if !xvalues.contains((reflectionX + reflectionX - x).toInt) => return false
            case _ =>
          }
        case _ =>
      }
      true
    }
  }

}
