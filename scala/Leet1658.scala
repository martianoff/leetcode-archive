object Leet1658 extends App {

  import scala.collection.mutable

  object Solution {
    def minOperations(nums: Array[Int], x: Int): Int = {
      val queue = mutable.Queue.empty[Bfs]
      queue.enqueue(
        Bfs(start = 1, end = nums.length - 1, total = x - nums(0), operations = 1)
      )
      queue.enqueue(
        Bfs(start = 0, end = nums.length - 2, total = x - nums(nums.length - 1), operations = 1)
      )
      while (queue.nonEmpty) {
        queue.dequeue() match {
          case Bfs(_, _, total, operations) if total == 0 =>
            return operations
          case Bfs(start, end, total, operations) if total > 0 && start <= end =>
            queue.enqueue(
              Bfs(start = start + 1, end = end, total = total - nums(start), operations = operations + 1)
            )
            queue.enqueue(
              Bfs(start = start, end = end - 1, total = total - nums(end), operations = operations + 1)
            )
          case _ =>
        }
      }
      -1
    }

    case class Bfs(start: Int, end: Int, total: Int, operations: Int)
  }

}
