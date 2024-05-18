object Leet991 extends App {

  import scala.math._

  object Solution {
    def log2(x: Double): Double = log10(x) / log10(2)

    def brokenCalc(startValue: Int, target: Int): Int = {
      if (startValue > target) return startValue - target
      var multiplies = 0
      var value = startValue
      while (value < target) {
        value *= 2
        multiplies += 1
      }
      println(s"value ${value} multiplies ${multiplies}")
      (multiplies + countUpOps(value - target, multiplies, target)) min countDownOps(value, multiplies, 1, target)
    }

    def countDownOps(startValue: Int, multiplies: Int, power: Int, target: Int): Int = {
      val opsFromDown = (multiplies - 1) + ((startValue / pow(2, power).toInt - ((target + 1) / pow(2, power).toInt)) + power + (if (target % 2 == 0) 0 else 1))
      println(s"downops start ${startValue / pow(2, power).toInt} multiplies ${multiplies} power ${power} ${opsFromDown}")
      //((6-5)+1)) = 0
      if (multiplies > 1) {
        opsFromDown min countDownOps(startValue, multiplies - 1, power + 1, target)
      } else opsFromDown
    }

    def countUpOps(diff: Int, multiplies: Int, target: Int): Int = {
      println(s"diff ${diff}")
      if (multiplies == 0) return diff
      diff match {
        case 0 => 0
        case 1 => 1
        case n => log2(n).toInt match {
          case closest2log =>
            // we can't deduct more than multiplies times
            val closestPow = pow(2, closest2log min multiplies).toInt
            println(s"closest2log ${closest2log}")
            println(s"power to build diff ${closestPow}")
            1 + countUpOps(diff - closestPow, multiplies - 1, target)
        }
      }
    }
  }

  /**
   * 3
   * 10
   * */

  /**
   * 363
   * 811
   * (363-160)*2*2-1=163
   * */

  /**
   * 63
   * 81
   * (63-22)*2-1=24
   * */

  /**
   * 68
   * 71
   * (68-36)*2-1=34
   * */

  /**
   * 68
   * 70
   * */

  /**
   * 12
   * 323
   * */
}
