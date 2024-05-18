object Leet402 extends App {
  object Solution {

    import scala.collection.mutable

    object Solution {

      def removeKdigits(num: String, k: Int): String = {
        val pick = num.length - k
        if (pick == 0) {
          return "0"
        }
        bestPick(
          pick = pick,
          num = num
        )
      }

      def bestPick(pick: Int, num: String): String = {
        val smallest = mutable.Stack.empty[Int]
        var remaining = num.length
        (0 until num.length).foreach { p =>
          val digit = num(p) - '0'
          // we can only replace leading nums if we have enough options
          while (smallest.headOption.exists(_ > digit) && remaining + smallest.size > pick) {
            smallest.pop()
          }
          if (smallest.size < pick) {
            smallest.push(digit)
          }
          remaining -= 1
        }
        smallest
          .reverse
          .zipWithIndex
          // remove leading zeroes except the last one
          .dropWhile {
            case (c, i) if c == 0 && i != smallest.length - 1 => true
            case _ => false
          }
          .map(_._1)
          .mkString("")
      }
    }
  }
}