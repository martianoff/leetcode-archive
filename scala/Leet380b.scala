object Leet380b extends App {

  import scala.collection.mutable

  class RandomizedSet() {

    val indexedMap = mutable.Map.empty[Int, Int]
    val reverseIndexedMap = mutable.Map.empty[Int, Int]
    val randomGen = new scala.util.Random

    def insert(`val`: Int): Boolean = {
      reverseIndexedMap.get(`val`) match {
        case Some(_) =>
          false
        case _ =>
          reverseIndexedMap(`val`) = indexedMap.size
          indexedMap(indexedMap.size) = `val`
          true
      }
    }

    def remove(`val`: Int): Boolean = {
      reverseIndexedMap.get(`val`) match {
        case Some(oldIndex) =>
          swap(oldIndex, indexedMap.size - 1)
          reverseIndexedMap -= `val`
          indexedMap -= indexedMap.size - 1
          true
        case _ =>
          false
      }
    }

    private def swap(index1: Int, index2: Int): Unit = {
      val oldVal = indexedMap(index1)
      indexedMap(index1) = indexedMap(index2)
      indexedMap(index2) = oldVal
      reverseIndexedMap(indexedMap(index2)) = index2
      reverseIndexedMap(indexedMap(index1)) = index1
    }

    def getRandom(): Int = {
      indexedMap(randomGen.between(0, indexedMap.size))
    }

  }
}
