object Leet380a extends App {

  import scala.collection.mutable
  import scala.util.chaining._

  val t = new RandomizedSet()

  class RandomizedSet() {

    val storage: mutable.LinkedHashSet[Int] = mutable.LinkedHashSet.empty[Int]
    val randomizer = new scala.util.Random


    def insert(`val`: Int): Boolean = {
      !storage.contains(`val`)
        .tap(_ => storage += `val`)
    }

    def remove(`val`: Int): Boolean = {
      storage.contains(`val`)
        .tap(_ => storage -= `val`)
    }

    def getRandom(): Int = {
      val index = randomizer.between(0, storage.size)
      storage.slice(index, index + 1).head
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
