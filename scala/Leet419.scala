import scala.collection.mutable

object Leet419 extends App {

  class Ship(y: Int, x: Int, board: Array[Array[Char]]) {
    val points: mutable.Queue[(Int, Int)] = mutable.Queue.empty[(Int, Int)]
    // explore vertical ships
    while (y < board.length && board(y + 1)(x) == 'X') {
      points.enqueue((y + 1, x))
    }
    // explore horizontal ships
    while (x < board(0).length && board(y)(x + 1) == 'X') {
      points.enqueue((y, x + 1))
    }
  }

  object Solution {
    def countBattleships(board: Array[Array[Char]]): Int = {
      val visited: mutable.Map[Int, mutable.Map[Int, Boolean]] = mutable.Map.empty[Int, mutable.Map[Int, Boolean]]
      board.zipWithIndex.foreach {
        case (_, y) => visited += (y -> mutable.Map.empty[Int, Boolean])
      }
      var ships: Int = 0
      board.zipWithIndex.foreach {
        case (row, y) =>
          row.zipWithIndex.foreach {
            case (_, x) =>
              if (!visited(y).contains(x)) {
                visited(y) += (x -> true)
                (y, x, board) match {
                  case Ship(s) =>
                    while (s.points.nonEmpty) {
                      s.points.dequeue() match {
                        case (py, px) => visited(py) += (px -> true)
                      }
                    }
                    ships += 1
                }
              }
          }
      }
      ships
    }
  }

  object Ship {
    def unapply(boardAndPos: (Int, Int, Array[Array[Char]])): Option[Ship] = {
      boardAndPos match {
        case (y: Int, x: Int, board: Array[Array[Char]]) =>
          if (board(y)(x) == 'X') {
            Some(new Ship(y, x, board))
          } else {
            None
          }
        case _ => None
      }
    }
  }

  println("")
}
