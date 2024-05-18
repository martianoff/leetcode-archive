object Leet37 extends App {

  object Solution {
    def solveSudoku(board: Array[Array[Char]]): Unit = {
      // calculate possible moves in all directions
      val horizontal = Vector.from((0 until 9).map {
        lineNumber =>
          ('1' to '9').toSet -- board(lineNumber).toSet
      })
      val vertical = Vector.from((0 until 9).map {
        colNumber =>
          ('1' to '9').toSet -- ((0 until 9).map {
            lineNumber =>
              board(lineNumber)(colNumber)
          }.toSet)
      })
      val blocks =
        Vector.from((0 until 3).map {
          blockNumberY =>
            Vector.from((0 until 3).map { blockNumberX =>
              ('1' to '9').toSet -- ((((blockNumberY * 3) until (blockNumberY + 1) * 3).foldLeft(Set.empty[Char]) {
                case (aggr, lineNumber) =>
                  aggr ++ (((blockNumberX * 3) until (blockNumberX + 1) * 3).foldLeft(Set.empty[Char]) {
                    case (aggr, colNumber) =>
                      aggr + board(lineNumber)(colNumber)
                  })
              }))
            })
        })
      val emptyCells = board.zipWithIndex.foldLeft(List.empty[EmptyCell]) {
        case (aggr, (row, y)) =>
          aggr ++ (row.zipWithIndex.foldLeft(List.empty[EmptyCell]) {
            case (aggr, (v, x)) if v == '.' => EmptyCell(y, x) :: aggr
            case (aggr, _) => aggr
          })
      }
      search(board, Step(emptyCells,
        Vector.from(board.map(Vector.from(_))),
        horizontal,
        vertical,
        blocks))
    }

    def search(finalBoard: Array[Array[Char]], step: Step): Boolean = {
      step match {
        // solved
        case Step(Nil, board, _, _, _) =>
          board.zipWithIndex.foreach {
            case (row, y) => row.zipWithIndex.foreach {
              case (v, x) => finalBoard(y)(x) = v
            }
          }
          true
        // have remaining cells
        case Step(EmptyCell(y, x) :: remainingEmptyCells, board, horizontal, vertical, blocks) =>
          // iterate through all possible combinations
          // we need to make sure that it satisfy all three conditions: vertical, horizontal, blocks
          ('1' to '9').foreach {
            case cellValue if horizontal(y).contains(cellValue) &&
              vertical(x).contains(cellValue) &&
              blocks(y / 3)(x / 3).contains(cellValue) =>
              if (search(finalBoard, Step(remainingEmptyCells,
                board.updated(y, board(y).updated(x, cellValue)),
                horizontal.updated(y, horizontal(y) - cellValue),
                vertical.updated(x, vertical(x) - cellValue),
                blocks.updated(y / 3, blocks(y / 3).updated(x / 3, blocks(y / 3)(x / 3) - cellValue))))) {
                return true
              }
            case _ =>
          }
          false
      }
    }

    case class EmptyCell(y: Int, x: Int)

    // state machine
    case class Step(
                     emptyCells: List[EmptyCell],
                     board: Vector[Vector[Char]],
                     horizontal: Vector[Set[Char]],
                     vertical: Vector[Set[Char]],
                     blocks: Vector[Vector[Set[Char]]])


  }
}
