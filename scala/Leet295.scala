import scala.collection.mutable

object Leet295 extends App {

  class MedianFinder() {

    private val leftStorage = mutable.PriorityQueue.empty[Int]
    private val rightStorage = mutable.PriorityQueue.empty[Int](ReverseOrdering)

    def addNum(num: Int) {
      rightStorage.enqueue(num)
      // rebalance
      leftStorage.enqueue(rightStorage.dequeue())
      // maintain size of left, left size shouldn't exceed right size by more than 1
      if (leftStorage.size - 1 > rightStorage.size) {
        rightStorage.enqueue(leftStorage.dequeue)
      }
    }

    def findMedian(): Double = {
      if (leftStorage.size > rightStorage.size) {
        leftStorage.head
      } else {
        (leftStorage.head + rightStorage.head).toDouble / 2
      }
    }

    object ReverseOrdering extends Ordering[Int] {
      def compare(a: Int, b: Int) = b compare a
    }

  }

}
