object Leet755 extends App {

  import scala.collection.mutable

  object Solution {
    def pourWater(heights: Array[Int], volume: Int, k: Int): Array[Int] = {
      (0 until volume).foreach {
        _ => pourOneVolume(heights, k)
      }
      heights
    }

    def pourOneVolume(heights: Array[Int], k: Int): Unit = {
      (leftGap(heights, k), rightGap(heights, k)) match {
        case (Some(l), _) =>
          // pour left
          pourOneVolume(heights, l)
        case (_, Some(r)) =>
          // pour right
          pourOneVolume(heights, r)
        case _ =>
          // pour at k
          heights(k) += 1
      }
    }

    def leftGap(heights: Array[Int], k: Int): Option[Int] = {
      (k - 1 to 0 by -1).foreach {
        case pos if heights(pos) < heights(k) => return Some(pos)
        case pos if heights(pos) > heights(k) => return None
        case _ =>
      }
      None
    }

    def rightGap(heights: Array[Int], k: Int): Option[Int] = {
      (k + 1 until heights.length).foreach {
        case pos if heights(pos) < heights(k) => return Some(pos)
        case pos if heights(pos) > heights(k) => return None
        case _ =>
      }
      None
    }
  }

}
