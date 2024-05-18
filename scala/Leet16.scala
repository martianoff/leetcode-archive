object Leet16 extends App {

  import scala.collection.mutable

  object Solution {
    def threeSumClosest(nums: Array[Int], target: Int): Int = {
      val storage = mutable.TreeMap.from(
        nums.groupBy(v => v).map {
          case (v, list) => (v, list.length)
        }
      )
      nums.combinations(2).map {
        case Array(a, b) =>
          storage
            .filter {
              case (k, v) if a == b && (k != a || v > 2) => true
              case (k, v) if a != b && (k != a || v > 1) && (k != b || v > 1) => true
              case _ => false
            }
            .range(
              (target - (a + b)) min ((a + b) - target),
              ((target - (a + b)) max ((a + b) - target)) + 1
            ) match {
            case result =>
              (result.headOption, result.lastOption) match {
                case (Some((hc, _)), Some((lc, _)))
                  if (target - (a + b + hc)).abs < (target - (a + b + lc)).abs =>
                  (a + b + hc)
                case (Some((_, _)), Some((lc, _))) =>
                  (a + b + lc)
                case _ => 99999999
              }
          }
      }.reduceLeft[Int] {
        case (a, b) if (target - a).abs < (target - b).abs => a
        case (_, b) => b
      }
    }
  }

}
