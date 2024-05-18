object Leet2013 extends App {

  import scala.collection.mutable

  class DetectSquares() {

    val xAxisPoints: mutable.Map[Int, mutable.Map[Int, Int]] = mutable.Map.empty[Int, mutable.Map[Int, Int]]
    val yAxisPoints: mutable.Set[Int] = mutable.Set.empty[Int]

    def add(point: Array[Int]) {
      (point(0), point(1)) match {
        case (x, y) =>
          if (xAxisPoints.contains(x)) {
            if (xAxisPoints(x).contains(y)) {
              xAxisPoints(x)(y) = xAxisPoints(x)(y) + 1
            } else {
              xAxisPoints(x) += (y -> 1)
            }
          } else {
            xAxisPoints += (x -> mutable.Map(y -> 1))
          }
          yAxisPoints += y
      }
    }

    def count(point: Array[Int]): Int = {
      (point(0), point(1)) match {
        case (x, y) =>
          if (xAxisPoints.contains(x) && yAxisPoints.contains(y)) {
            xAxisPoints(x).foldLeft(0) {
              case (total, (otherY, q1)) if (otherY != y) =>
                (otherY - y) match {
                  case size =>
                    (
                      // try to build to the right
                      xAxisPoints.get(x + size),
                      // try to build to the left
                      xAxisPoints.get(x - size)
                    ) match {
                      // can build both ways
                      case (Some(right), Some(left)) =>
                        (
                          right.get(otherY),
                          right.get(y),
                          left.get(otherY),
                          left.get(y)
                        ) match {
                          case (qr2, qr3, ql2, ql3) =>
                            total +
                              q1 * qr2.getOrElse(0) * qr3.getOrElse(0) +
                              q1 * ql2.getOrElse(0) * ql3.getOrElse(0)
                          case _ => total
                        }
                      // can build to the right
                      case (Some(right), None) =>
                        (
                          right.get(otherY),
                          right.get(y)
                        ) match {
                          case (Some(q2), Some(q3)) => total + q1 * q2 * q3
                          case _ => total
                        }
                      // can build to the left
                      case (None, Some(left)) =>
                        (
                          left.get(otherY),
                          left.get(y)
                        ) match {
                          case (Some(q2), Some(q3)) => total + q1 * q2 * q3
                          case _ => total
                        }
                      case (None, None) => total
                    }
                }
              // skip zero size squares
              case (total, _) => total
            }
          } else {
            0
          }
      }
    }

  }


}
