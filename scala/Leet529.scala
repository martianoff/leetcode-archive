import scala.collection.immutable.TreeSet

object Leet529 extends App {

  import scala.util.chaining._
  import scala.collection.mutable

  object Solution {
    val adjacentCells: Seq[(Int, Int)] = List((-1, 0), (-1, -1), (0, -1), (1, -1), (1, 0), (1, 1), (0, 1), (-1, 1))

    def updateBoard(board: Array[Array[Char]], click: Array[Int]): Array[Array[Char]] = {
      val visited = mutable.Set.empty[(Int, Int)]
      board.tap { _ =>
        click match {
          case Array(y, x) if board(y)(x) == 'M' =>
            board(y)(x) = 'X'
          case Array(y, x) if board(y)(x) == 'E' =>
            revealEmptySpace(board, (y, x), visited)
        }
      }
    }

    private def revealEmptySpace(board: Array[Array[Char]], emptySpace: (Int, Int), visited: mutable.Set[(Int, Int)]): Unit = {
      emptySpace match {
        case (y, x) if board(y)(x) == 'E' && !visited.contains((y, x)) =>
          visited += (y -> x)
          board(y)(x) = adjacentCells.collect {
            case (yOffset, xOffset) if
              y + yOffset >= 0 &&
                x + xOffset >= 0 &&
                y + yOffset < board.length &&
                x + xOffset < board(0).length
              =>
              board(y + yOffset)(x + xOffset) match {
                case 'M' => 1
                case _ =>
                  0
              }
          }.sum match {
            case 0 =>
              // if there are no bombs around reveal surrounding spaces
              adjacentCells.foreach {
                case (yOffset, xOffset) if
                  y + yOffset >= 0 &&
                    x + xOffset >= 0 &&
                    y + yOffset < board.length &&
                    x + xOffset < board(0).length &&
                    board(y + yOffset)(x + xOffset) == 'E' &&
                    !visited.contains((y + yOffset, x + xOffset))
                  =>
                  revealEmptySpace(board, (y + yOffset, x + xOffset), visited)
                case _ =>
              }
              'B'
            case n => n.toString.head
          }
        case _ =>
      }
    }
  }

}
