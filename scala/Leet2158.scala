object Leet2158 extends App {

  object Solution {
    def amountPainted(paint: Array[Array[Int]]): Array[Int] = {
      val painted = Array.fill(50000)(0)
      paint.map {
        case Array(start, end) =>
          var pos = start
          var sum = 0
          while (pos < end) {
            // jump to the next point or use the shortcut
            val next = (pos + 1) max painted(pos)
            // count if not yet painted
            if (painted(pos) == 0) {
              sum += 1
            }
            // creates a pointer to the end of interval
            painted(pos) = painted(pos) max end
            pos = next
          }
          sum
      }
    }
  }

}
