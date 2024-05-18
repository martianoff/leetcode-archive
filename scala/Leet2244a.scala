object Leet2244a extends App {

  object Solution {

    // math solution
    def minimumRounds(tasks: Array[Int]): Int = {
      tasks.groupBy {
          v => v
        }
        .map {
          case (v, arr) => (v, arr.length)
        }
        .map {
          case (_, numberOfTasks) => minRounds(numberOfTasks)
        }
        .foldLeft(0) {
          case (sum, Some(r)) => r + sum
          case (_, None) => return -1
        }
    }

    def minRounds(numberOfTasks: Int): Option[Int] = {
      if (numberOfTasks == 1) {
        None
      } else if (numberOfTasks == 2 || numberOfTasks == 3) {
        Some(1)
      } else if (numberOfTasks % 3 == 0) {
        Some(numberOfTasks / 3)
      } else {
        Some(numberOfTasks / 3 + 1)
      }
    }
  }

}
