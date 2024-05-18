object Leet1834 extends App {

  import scala.collection.mutable

  object Solution {
    def getOrder(tasks: Array[Array[Int]]): Array[Int] = {
      val taskPool = mutable.PriorityQueue.from(tasks.zipWithIndex.map { case (Array(s, d), i) => Task(
        id = i,
        start = s,
        duration = d
      )
      })(TaskAvailabilityOrdering)
      var time = taskPool.head.start
      val availableTasks = mutable.PriorityQueue.empty[Task](TaskPriorityOrdering)
      val result = mutable.Queue.empty[Int]
      while (taskPool.nonEmpty || availableTasks.nonEmpty) {
        // get tasks available by time from taskPool and move the to availability list
        while (taskPool.headOption.exists(_.start <= time)) {
          availableTasks.enqueue(taskPool.dequeue)
        }
        // if no tasks available jump to next available task
        if (availableTasks.isEmpty) {
          time = taskPool.head.start
        } else {
          // execute task
          availableTasks.dequeue() match {
            case Task(id, start, duration) =>
              result.enqueue(id)
              time += duration
          }
        }
      }
      result.toArray
    }

    case class Task(id: Int, start: Int, duration: Int)

    object TaskAvailabilityOrdering extends Ordering[Task] {
      def compare(a: Task, b: Task) = {
        b.start compare a.start
      }
    }

    object TaskPriorityOrdering extends Ordering[Task] {
      def compare(a: Task, b: Task): Int = {
        if (b.duration == a.duration) {
          b.id compare a.id
        } else {
          b.duration compare a.duration
        }
      }
    }
  }
}
