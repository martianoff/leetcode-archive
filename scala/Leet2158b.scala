object Leet2158b extends App {

  object Solution {
    def amountPainted(paint: Array[Array[Int]]): Array[Int] = {
      val painted = Array.fill(50000)(0)
      paint.map {
        case Array(start, end) =>
          var pos = start
          var sum = 0
          while (pos < end) {
            val next = (pos + 1) max painted(pos)
            // not yet painted
            if (painted(pos) == 0) {
              sum += 1
            }
            painted(pos) += 1
            pos = next
          }
          sum
      }
    }
  }

}
