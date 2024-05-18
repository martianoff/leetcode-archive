object Leet348 extends App {

  class TicTacToe(_n: Int) {

    val desk = Array.fill(_n)(Array.fill(_n)(0))

    def move(row: Int, col: Int, player: Int): Int = {
      desk(row)(col) = player
      //check horizontal
      (0 until _n).foldLeft(true) {
        case (win, x) => win && desk(row)(x) == player
      } match {
        case true =>
          return player
        case _ =>
      }
      //check vertical
      (0 until _n).foldLeft(true) {
        case (win, y) => win && desk(y)(col) == player
      } match {
        case true =>
          return player
        case _ =>
      }
      // placed on diagonal
      if (col == row) {
        //check diagonal 1
        var x = col
        var y = row
        var win = true
        while (y >= 0 && x >= 0) {
          win = win && desk(y)(x) == player
          x -= 1
          y -= 1
        }
        x = col
        y = row
        while (y < _n && x < _n) {
          win = win && desk(y)(x) == player
          x += 1
          y += 1
        }
        win match {
          case true =>
            return player
          case _ =>
        }
      }
      if (col == _n - row - 1) {
        //check diagonal 2
        var x = col
        var y = row
        var win = true
        while (y >= 0 && x < _n) {
          win = win && desk(y)(x) == player
          x += 1
          y -= 1
        }
        x = col
        y = row
        while (y < _n && x >= 0) {
          win = win && desk(y)(x) == player
          x -= 1
          y += 1
        }
        win match {
          case true =>
            return player
          case _ =>
        }
      }
      0
    }

  }

  /**
   * Your TicTacToe object will be instantiated and called as such:
   * var obj = new TicTacToe(n)
   * var param_1 = obj.move(row,col,player)
   */

}
