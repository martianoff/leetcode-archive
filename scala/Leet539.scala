import scala.collection.immutable.{AbstractSeq, LinearSeq, TreeSet}

object Leet539 extends App {
  object Solution {
    private val DayMax: Int = 24 * 60

    def findMinDifference(timePoints: List[String]): Int = {
      TreeSet.from(timePoints.map {
        _.split(":").toSeq match {
          case Seq(hr, min) => hr.toInt * 60 + min.toInt
        }
      }) match {
        case uniqueTimePoints if uniqueTimePoints.size >= timePoints.size =>
          // either two consequent values or first + last
          uniqueTimePoints.toSeq ++ Seq(uniqueTimePoints.head, uniqueTimePoints.last) match {
            case s => s.sliding(2, 1).map {
              case Seq(a, b) =>
                Seq(
                  // clockwise
                  b - a,
                  a - b,
                  // counter clockwise
                  b + (DayMax - a),
                  a + (DayMax - b)
                ).filter(_ >= 0).min
            }.min
          }
        // 2 equal values in a list
        case _ => 0
      }
    }
  }

  println(Solution.findMinDifference(List("23:59", "00:00")))
  println(Solution.findMinDifference(List("00:00", "23:59", "00:00")))
  println(Solution.findMinDifference(List("10:59", "12:00", "10:30")))
  println(Solution.findMinDifference(List("11:59", "10:59", "12:00", "10:30")))
  println(Solution.findMinDifference(List("11:59", "14:59", "12:33", "10:30")))
  println(Solution.findMinDifference(List("12:12", "00:13"))) //719
  println(Solution.findMinDifference(List("00:00", "04:00", "22:00"))) //120

}
