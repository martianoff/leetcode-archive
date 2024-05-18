object Leet51 {
  def solveNQueens(n: Int): List[List[String]] = {
    dfs(
      n = n,
      queen = 0,
      freeCols = (0 until n).toSet,
      placed = Vector.fill(n)(-1)
    ).map {
      data =>
        val picture = Array.fill(n)(Array.fill(n)("."))
        data.zipWithIndex.foreach {
          case (y, x) => picture(y)(x) = "Q"
        }
        picture.map(_.mkString).toList
    }
  }

  // placed: key = x, value = y
  def dfs(n: Int, queen: Int, freeCols: Set[Int], placed: Vector[Int]): List[Vector[Int]] = {
    if (queen == n) {
      List(placed)
    } else {
      freeCols.collect {
        case x if valid(placed, n, queen, x) =>
          dfs(
            n = n,
            queen = queen + 1,
            freeCols = freeCols - x,
            placed = placed.updated(x, queen)
          )
      }.toList.flatten
    }
  }

  def valid(placed: Vector[Int], n: Int, queen: Int, x: Int): Boolean = {
    (0 until n).foreach {
      // check diagonals if deltaX == deltaY and queen was placed
      case anotherX if anotherX != x &&
        placed(anotherX) != -1 &&
        (placed(anotherX) - queen).abs == (anotherX - x).abs =>
        return false
      case _ =>
    }
    true
  }
}