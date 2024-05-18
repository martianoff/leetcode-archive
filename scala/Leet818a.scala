object Leet818a extends App {

  import scala.collection.mutable

  case class Step(position: Int, speed: Int) {
    lazy val forward: Step = Step(position + speed, speed * 2)
    lazy val backward: Step = Step(position, if (speed > 0) {
      -1
    } else {
      1
    })
  }

  object Solution {
    def racecar(target: Int): Int = {
      val queue = mutable.Queue.empty[(Step, Int)]
      queue.enqueue((Step(position = 0, speed = 1), 0))

      while (queue.nonEmpty) {
        queue.dequeue() match {
          case (step, count) =>
            if (step.position == target) {
              println("queue size = " + queue.size)
              return count
            } else if ((step.forward.position > target && step.speed > 0) || (step.forward.position < target && step.speed < 0)) {
              //R
              queue.enqueue((step.backward, count + 1))
              //A
              queue.enqueue((step.forward, count + 1))
            } else {
              //A
              queue.enqueue((step.forward, count + 1))
            }
        }
      }
      -1
    }
  }

  println(Solution.racecar(3)) //2
  println(Solution.racecar(5)) //7
  println(Solution.racecar(6)) //5
  println(Solution.racecar(4)) //5
}
