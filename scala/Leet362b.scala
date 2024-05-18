object Leet362b extends App {

  import scala.collection.mutable

  class HitCounter() {

    /**
     * LinkedHashMap allows us to keep memory size fixed even if we keep getting hits many times for the same timestamp
     */
    val storage = mutable.LinkedHashMap.empty[Int, Int].withDefaultValue(0)
    var totalHits = 0

    def hit(timestamp: Int): Unit = {
      trim(timestamp)
      storage(timestamp) += 1
      totalHits += 1
    }

    def getHits(timestamp: Int): Int = {
      trim(timestamp)
      totalHits
    }

    private def trim(timestamp: Int): Unit = {
      while (storage.nonEmpty && storage.headOption.exists(_._1 <= timestamp - 300)) {
        // we can use head with LinkedHashMap
        totalHits -= storage.head._2
        storage -= storage.head._1
      }
    }

  }


}
