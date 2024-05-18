object Leet488 extends App {

  import scala.collection.mutable
  import scala.util.chaining._

  object Solution {
    val cache = mutable.Map.empty[(List[Char], Map[Int, Char], Int), Option[Int]]
    var bestSoFar: Option[Int] = None

    def findMinStep(board: String, hand: String): Int = {
      bestSoFar = None
      dfs(
        board = board.toList,
        hand = hand.zipWithIndex.map { case (c, i) => (i, c) }.toMap,
        picks = 0
      ).getOrElse(-1)
    }

    def dfs(board: List[Char], hand: Map[Int, Char], picks: Int): Option[Int] = {
      if (cache.contains((board, hand, picks))) {
        return cache((board, hand, picks))
      }
      if (board.isEmpty) {
        bestSoFar = Some(picks)
        return Some(picks)
      }
      if (hand.isEmpty) {
        return None
      }
      if (bestSoFar.exists(_ < picks)) {
        return None
      }
      hand.flatMap {
          case (pos, ball) =>
            (0 to board.size).collect {
              case insertionPoint =>
                val insertedBoard = if (insertionPoint == 0) {
                  ball :: board
                } else {
                  board.slice(0, insertionPoint) ++
                    (ball :: board.slice(insertionPoint, board.length))
                }
                // optimize board while possible
                var cleanBoard = insertedBoard
                var prevBoard = insertedBoard
                do {
                  prevBoard = cleanBoard
                  cleanBoard =
                    prevBoard.foldLeft(List.empty[(Char, Int)]) {
                        case (Nil, char) => List((char, 1))
                        case ((prevChar, count) :: tail, char) if char == prevChar => (prevChar, count + 1) :: tail
                        case ((prevChar, count) :: tail, char) => (char, 1) :: (prevChar, count) :: tail
                      }.collect {
                        case (char, count) if count < 3 => (0 until count).map(_ => char)
                      }
                      .flatten
                      .reverse
                } while (prevBoard != cleanBoard)
                dfs(
                  board = cleanBoard,
                  hand = hand - pos,
                  picks = picks + 1
                )
            }
        }.collect {
          case Some(r) => r
        }
        .minOption
        .tap {
          r => cache((board, hand, picks)) = r
        }
    }
  }

}
