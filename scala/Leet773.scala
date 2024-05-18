object Leet773 extends App {

  import scala.collection.mutable

  object Solution {
    val directions = List((0, 1), (0, -1), (-1, 0), (1, 0))

    def slidingPuzzle(board: Array[Array[Int]]): Int = {
      val cache = mutable.Set.empty[String]
      // locate zero
      var zeroPos = Position(-1, -1)
      board.zipWithIndex.foreach {
        case (row, y) => row.zipWithIndex.foreach {
          case (value, x) => if (value == 0) {
            zeroPos = Position(x, y)
          }
        }
      }
      // bfs
      val queue = mutable.Queue.empty[(Position, Array[Array[Int]], Int)]
      queue.enqueue((zeroPos, board, 0))
      while (queue.nonEmpty) {
        queue.dequeue() match {
          case (pos, boardState, turns) =>
            val strBoard = stringifyBoard(boardState)
            if (strBoard == "123450") {
              return turns
            }
            if (!cache.contains(strBoard)) {
              cache += strBoard
              directions.foreach {
                case (xOffset, yOffset) if pos.x + xOffset >= 0 && pos.x + xOffset <= 2 &&
                  pos.y + yOffset >= 0 && pos.y + yOffset <= 1 =>
                  val newPos = Position(pos.x + xOffset, pos.y + yOffset)
                  queue.enqueue(
                    (newPos, boardAfterMove(pos, newPos, boardState), turns + 1)
                  )
                case _ =>
              }
            }
        }
      }
      -1
    }

    def boardAfterMove(oldPos: Position, newPos: Position, board: Array[Array[Int]]): Array[Array[Int]] = {
      val newBoard = board.map(_.clone())
      val oldVal = newBoard(oldPos.y)(oldPos.x)
      newBoard(oldPos.y)(oldPos.x) = newBoard(newPos.y)(newPos.x)
      newBoard(newPos.y)(newPos.x) = oldVal
      newBoard
    }

    def stringifyBoard(board: Array[Array[Int]]): String = {
      board.map(row => row.mkString("")).mkString("")
    }

    case class Position(x: Int, y: Int)
  }
}
