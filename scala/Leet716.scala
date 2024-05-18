object Leet716 extends App {

  import scala.collection.mutable
  import scala.collection.immutable

  class MaxStack() {

    // ordered treemap from value to ordered set of positions (provides mapping from max value to ids)
    private val maxStack = mutable.TreeMap.empty[Int, immutable.TreeSet[Int]]
    // ordered treemap from position to value (provides mutable stack-like operations)
    private val posStack = mutable.TreeMap.empty[Int, Int]
    private var id = 0

    def push(x: Int) {
      posStack += (id -> x)
      if (!maxStack.contains(x)) {
        maxStack(x) = immutable.TreeSet.empty[Int]
      }
      maxStack(x) = maxStack(x) + id
      id += 1
    }

    def pop(): Int = {
      deleteElement(idToPop = posStack.last._1, value = posStack.last._2)
    }

    private def deleteElement(idToPop: Int, value: Int): Int = {
      if (maxStack(value).size > 1) {
        maxStack(value) -= idToPop
      } else {
        maxStack -= value
      }
      posStack -= idToPop
      value
    }

    def top(): Int = {
      posStack
        .last
        ._2
    }

    def peekMax(): Int = {
      maxStack
        .last
        ._1
    }

    def popMax(): Int = {
      deleteElement(idToPop = maxStack.last._2.last, value = maxStack.last._1)
    }

  }
}
