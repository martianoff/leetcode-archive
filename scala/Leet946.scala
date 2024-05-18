object Leet946 extends App {

  import scala.collection.mutable

  object Solution {
    def validateStackSequences(pushed: Array[Int], popped: Array[Int]): Boolean = {
      val stack = mutable.Stack.empty[Int]
      val target = mutable.Queue.from(popped)
      pushed.foreach { v =>
        stack.push(v)
        while (stack.headOption.exists(stackValue => target.headOption.contains(stackValue))) {
          target.dequeue()
          stack.pop()
        }
      }
      stack.isEmpty
    }
  }

}
