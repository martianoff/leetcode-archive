object Leet362 extends App {

  import scala.collection.mutable

  class HitCounter() {

    val storage = mutable.Queue.empty[Int]

    var hits = 0

    def hit(timestamp: Int) {
      updateState(timestamp)
      storage.enqueue(timestamp)
      hits += 1
    }

    def getHits(timestamp: Int): Int = {
      updateState(timestamp)
      hits
    }

    private def updateState(timestamp: Int) {
      while (storage.nonEmpty && storage.head <= timestamp - 300) {
        storage.dequeue()
        hits -= 1
      }
    }

  }


}
