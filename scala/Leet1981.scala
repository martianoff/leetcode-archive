import scala.collection.mutable

object Leet1981 extends App {

  object Solution {

    def minimizeTheDifference(mat: Array[Array[Int]], target: Int): Int = {
      if (mat.length == 0) {
        return target
      }
      val stack = mutable.Stack.empty[Node]
      mat(0).zipWithIndex.foreach {
        case (value, column) => stack.push(Node(row = 0, col = column, sum = value))
      }
      var bestDiff = target
      while (stack.nonEmpty) {
        stack.pop() match {
          case Node(row, _, oldSum) if (row == mat.length - 1) =>
            bestDiff = bestDiff min (oldSum - target).abs
          case Node(row, col, oldSum) => {
            mat(row + 1).zipWithIndex.foreach {
              case (value, column) =>
                val total = oldSum + value
                // explore only if it can possibly improve the result
                if (total < target + bestDiff) {
                  stack.push(Node(
                    row = row + 1,
                    col = column,
                    sum = total))
                }
            }
          }
        }
      }
      bestDiff
    }

    case class Node(row: Int, col: Int, sum: Int)
  }

}
