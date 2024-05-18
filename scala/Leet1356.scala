import scala.util.chaining._

object Leet1356 extends App {
  def sortByBits(arr: Array[Int]): Array[Int] = {
    arr.tap {
      _.sortInPlaceWith {
        case (a, b) =>
          (bits(a), bits(b)) match {
            case (ba, bb) if ba == bb => a < b
            case (ba, bb) => ba < bb
          }
      }
    }
  }

  private def bits(n: Int): Int = {
    (0 until 32).map {
      // read 1 bit at bitNum
      case bitNum if ((n >> bitNum) & 0x1) == 1 => 1
      case _ => 0
    }.sum
  }
}