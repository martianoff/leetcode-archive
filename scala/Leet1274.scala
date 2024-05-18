object Leet1274 extends App {

  trait Sea {
    def hasShips(s1: Array[Int], s2: Array[Int]): Boolean
  }

  object Solution {
    def countShips(sea: Sea, topRight: Array[Int], bottomLeft: Array[Int]): Int = {
      (bottomLeft, topRight) match {
        case (Array(x1, y1), Array(x2, y2)) if x2 >= x1 && y2 >= y1 && sea.hasShips(Array(x2, y2), Array(x1, y1)) =>
          if (x1 == x2 && y1 == y2) {
            1
          } else {
            val midY = (y2 + y1) / 2
            val midX = (x2 + x1) / 2
            List(
              countShips(sea, Array(midX, midY), Array(x1, y1)),
              countShips(sea, Array(x2, midY), Array(midX + 1, y1)),
              countShips(sea, Array(midX, y2), Array(x1, midY + 1)),
              countShips(sea, Array(x2, y2), Array(midX + 1, midY + 1))
            ).sum
          }
        case _ => 0
      }
    }
  }

}
