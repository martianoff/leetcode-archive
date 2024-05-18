object Leet818 extends App {

  import scala.util.chaining._

  case class Step(position: Int, speed: Int) {
    def forward: Step = Step(position + speed, speed * 2)

    def backward: Step = Step(position, if (speed > 0) {
      -1
    } else {
      1
    })
  }

  object Solution {
    def racecar(target: Int): Int = {
      racecar(target = target, Step(position = 0, speed = 1))
    }

    def racecar(target: Int, step: Step): Int = {
      if (step.position == target) {
        0
      } else if (step.position > target && step.speed > 0 || step.position < target && step.speed < 0) {
        //R
        step.backward
          .pipe(racecar(target, _))
          .pipe(_ + 1)
      } else {
        //A
        step.forward
          .pipe(racecar(target, _))
          .pipe(_ + 1)
      }
    }
  }

  println(Solution.racecar(3)) //2
  println(Solution.racecar(5)) //7
  println(Solution.racecar(6)) //5
  println(Solution.racecar(4)) //5
}
