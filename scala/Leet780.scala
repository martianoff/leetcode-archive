object Leet780 extends App {

  import scala.collection.mutable
  import scala.util.chaining._

  object Solution {
    val cache = mutable.Map.empty[(Int, Int, Int, Int), Boolean]

    def reachingPoints(sx: Int, sy: Int, tx: Int, ty: Int): Boolean = {
      cached((sx, sy, tx, ty)) {
        if (sx == tx && sy == ty) {
          return true
        }
        if (sx > tx || sy > ty) {
          return false
        }
        reachingPoints(sx + sy, sy, tx, ty) || reachingPoints(sx, sx + sy, tx, ty)
      }
    }

    private def cached(key: (Int, Int, Int, Int))(cacheable: => Boolean): Boolean = {
      cache.get(key) match {
        case Some(cachedValue) => cachedValue
        case None =>
          cacheable.tap { result =>
            cache += (key -> result)
          }
      }
    }
  }

}
