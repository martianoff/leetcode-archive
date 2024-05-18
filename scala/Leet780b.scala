object Leet780b extends App {

  object Solution {
    def reachingPoints(sx: Int, sy: Int, tx: Int, ty: Int): Boolean = {
      var y = ty
      var x = tx
      while (y >= sy && x >= sx) {
        if (x == sx && y == sy) {
          return true
        }
        if (y > x) {
          y -= x
        } else {
          x -= y
        }
        println(s"x=${x},y=${y}")
      }
      false
    }
  }

}
