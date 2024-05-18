object Leet981 extends App {

  import scala.collection.mutable

  class TimeMap() {

    val storage = mutable.Map.empty[String, mutable.TreeMap[Int, String]]

    // O(log N)
    def set(key: String, value: String, timestamp: Int): Unit = {
      storage.get(key) match {
        case Some(treemap) =>
          treemap += (timestamp -> value)
        case None =>
          storage(key) = mutable.TreeMap(timestamp -> value)
      }
    }

    // O(log N)
    def get(key: String, timestamp: Int): String = {
      storage
        .get(key).flatMap(
          _
            .rangeTo(timestamp)
            .lastOption
            .map(_._2)
        )
        .getOrElse("")
    }

  }
}
