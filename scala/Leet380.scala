object Leet380 extends App {

  import scala.collection.mutable

  val t = new RandomizedSet()

  class RandomizedSet() {

    val storage: IndexedMap = IndexedMap()
    val randomizer = new scala.util.Random


    def insert(`val`: Int): Boolean = {
      storage.insert(`val`)
    }

    def remove(`val`: Int): Boolean = {
      storage.remove(`val`)
    }

    def getRandom(): Int = {
      storage.getByIndex(randomizer.between(0, storage.size))
    }

    case class IndexedMap() {
      private val data = mutable.Map.empty[Int, Int]
      private var indices = Vector[Int]()
      private var totalNumberOfElements = 0

      def size: Int = {
        totalNumberOfElements
      }

      def getByIndex(index: Int): Int = {
        indices(indexToReversedIndex(index))
      }

      def remove(v: Int): Boolean = {
        data.get(v) match {
          case Some(index) =>
            data -= v
            // swap the first element with removed element
            if (totalNumberOfElements > 0 && indexToReversedIndex(index) != 0) {
              val oldHeadValue = indices(0)
              indices = indices.updated(indexToReversedIndex(index), oldHeadValue).tail
              data(oldHeadValue) = index
            } else if (totalNumberOfElements > 0) {
              indices = indices.tail
            }
            totalNumberOfElements -= 1
            true
          case None =>
            false
        }
      }

      private def indexToReversedIndex(index: Int) = {
        totalNumberOfElements - index - 1
      }

      def insert(v: Int): Boolean = {
        data.get(v) match {
          case None =>
            data += (v -> totalNumberOfElements)
            // since in scala we can only pop the head
            // we need to keep reverse order to avoid reindexing
            indices = v +: indices
            totalNumberOfElements += 1
            true
          case Some(_) =>
            false
        }
      }
    }

  }

  t.insert(1)
  t.remove(2)
  t.insert(2)
  t.getRandom()
  t.remove(1)
  t.insert(2)
  println(t.getRandom())

  /*
          t.insert(0)
     t.insert(1)
     t.remove(0)
      t.insert(2)
      t.remove(1)
      println(t.getRandom())*/
}
