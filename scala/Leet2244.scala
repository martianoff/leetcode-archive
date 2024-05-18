object Leet2244 extends App {

  import scala.util.chaining._
  import scala.collection.mutable

  object Solution {
    val cache = mutable.Map.empty[Int, Option[Int]]

    def minimumRounds(tasks: Array[Int]): Int = {
      tasks.groupBy {
          v => v
        }
        .map {
          case (v, arr) => (v, arr.length)
        }
        .map {
          case (_, numberOfTasks) => minRounds(cache, numberOfTasks)
        }
        .foldLeft(0) {
          case (sum, Some(r)) => r + sum
          case (_, None) => return -1
        }
    }

    def minRounds(cache: mutable.Map[Int, Option[Int]], numberOfTasks: Int): Option[Int] = {
      if (cache.contains(numberOfTasks)) {
        return cache(numberOfTasks)
      }
      (
        if (numberOfTasks == 1) {
          None
        } else if (numberOfTasks == 2 || numberOfTasks == 3) {
          Some(1)
        } else {
          ((minRounds(cache, numberOfTasks - 3), minRounds(cache, numberOfTasks - 2)) match {
            case (Some(a), Some(b)) => Some(a min b)
            case (None, Some(b)) => Some(b)
            case (Some(a), None) => Some(a)
            case (None, None) => None
          }).map(_ + 1)
        }
        ).tap {
        r => cache(numberOfTasks) = r
      }
    }
  }

}
