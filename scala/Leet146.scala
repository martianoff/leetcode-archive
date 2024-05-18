object Leet146 extends App {

  import scala.collection.mutable

  class LRUCache(_capacity: Int) {

    val storage: mutable.Map[Int, Int] = mutable.LinkedHashMap.empty[Int, Int]

    def get(key: Int): Int = {
      storage.get(key) match {
        case Some(value) =>
          storage -= key
          storage += (key -> value)
          value
        case None =>
          -1
      }

    }

    def put(key: Int, value: Int) = {
      storage.get(key) match {
        case Some(value) =>
          storage -= key
        case None =>
      }
      storage += (key -> value)
      if (storage.size > _capacity) {
        storage -= storage.head._1
      }
      value
    }

  }

}