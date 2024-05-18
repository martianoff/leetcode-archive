

object Leet582 extends App {

  import scala.collection.mutable
  import scala.util.chaining._

  class Solution(_w: Array[Int]) {

    val storage = mutable.TreeMap.empty[Int, Int]
    val total = _w.zipWithIndex.foldLeft(0) {
      case (total, (weight, index)) =>
        (weight + total).tap {
          t => storage += (t -> index)
        }
    }

    val randomizer = scala.util.Random

    def pickIndex(): Int = {
      // between is excluding max
      val random = randomizer.nextInt(total) + 1
      storage.rangeFrom(random).head._2
    }

  }

}
