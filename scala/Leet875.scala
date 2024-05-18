object Leet875 extends App {

  object Solution {
    def minEatingSpeed(piles: Array[Int], h: Int): Int = {
      val maxPile = piles.max
      var left = (maxPile / h) max 1 max (piles.sum / h)
      var right = maxPile
      while (left < right) {
        val mid = left + (right - left) / 2
        if (timeForEatingPile(speed = mid, piles = piles) > h) {
          // try to increase speed
          left = mid + 1
        } else {
          // try to lower speed
          right = mid
        }
      }
      left
    }

    def timeForEatingPile(speed: Int, piles: Array[Int]) = {
      piles.map {
        case pile if pile % speed == 0 => pile / speed
        case pile => pile / speed + 1
      }.sum
    }
  }


}
