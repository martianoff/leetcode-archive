object Leet319 extends App {

  import scala.collection.mutable

  object Solution {
    def bulbSwitch(n: Int): Int = {
      val storage = mutable.BitSet.empty
      (1 to n).foreach {
        round =>
          (round - 1 until n by round).foreach {
            i =>
              storage(i) = !storage(i)
          }
      }
      storage.size
    }
  }

}
