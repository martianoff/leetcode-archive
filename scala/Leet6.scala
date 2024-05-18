import scala.collection.mutable

object Leet6 extends App {
  /*
      P   A   H   N
      A P L S I I G
      Y   I   R
   */
  def convert(s: String, numRows: Int): String = {
    if (s.length < numRows || numRows == 1) return s
    val queue = mutable.Queue.from(s.toCharArray)
    val output: mutable.Map[Int, mutable.Map[Int, Char]] = mutable.Map()
    var x = 0
    var y = 0
    var direction = 1
    var result = ""
    var width = 0
    while (queue.nonEmpty) {
      if (!output.contains(y))
        output += y -> mutable.Map()
      output(y) += x -> queue.dequeue()
      if (direction == 1) {
        y = y + 1
        if (y == numRows) {
          direction = -1
          y = y - 1
        }
      }
      if (direction == -1) {
        y = y - 1
        x = x + 1
        width = Math.max(width, x)
        if (y == -1) {
          direction = 1
          y = 1
          x = x - 1
        }
      }
    }
    for (y <- 0 until numRows) {
      for (x <- 0 until width) {
        result += output(y).getOrElse(x, "")
      }
    }
    result
  }

  //Input: s = "PAYPALISHIRING", numRows = 3
  //Output: "PAHNAPLSIIGYIR"
  println(Leet6.convert("PAYPALISHIRING", 3))

}
