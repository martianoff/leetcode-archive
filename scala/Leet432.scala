object Leet432 extends App {

  import scala.collection.mutable

  class AllOne() {

    val sortedMap = mutable.TreeMap.empty[Int, mutable.Set[String]]
    val map = mutable.Map.empty[String, Int].withDefaultValue(0)

    // O(log n)
    def inc(key: String) {
      val oldVal = map(key)
      // remove old value from tree
      sortedMap.get(oldVal) match {
        case Some(set) =>
          set -= key
          // remove empty keys
          if (set.isEmpty) {
            sortedMap -= oldVal
          }
        case None =>
      }
      map(key) += 1
      // add new value to tree
      sortedMap.get(oldVal + 1) match {
        case Some(set) => set += key
        case None => sortedMap(oldVal + 1) = mutable.Set(key)
      }
    }

    // O(log n)
    def dec(key: String) {
      val oldVal = map(key)
      // remove old value from tree
      sortedMap.get(oldVal) match {
        case Some(set) =>
          set -= key
          // remove empty keys
          if (set.isEmpty) {
            sortedMap -= oldVal
          }
        case None =>
      }
      if (oldVal - 1 > 0) {
        map(key) -= 1
        // add new value to tree
        sortedMap.get(oldVal - 1) match {
          case Some(set) => set += key
          case None => sortedMap(oldVal - 1) = mutable.Set(key)
        }
      } else {
        map -= key
      }
    }

    // O(log n)
    def getMaxKey(): String = {
      sortedMap.lastOption.map(_._2.head).getOrElse("")
    }

    // O(log n)
    def getMinKey(): String = {
      sortedMap.headOption.map(_._2.head).getOrElse("")
    }

  }

}
