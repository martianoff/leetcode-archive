object Leet353 extends App {

  import scala.collection.mutable

  sealed trait SnakeGameEntities

  case class Empty() extends SnakeGameEntities

  case class SnakeTail() extends SnakeGameEntities

  case class SnakeFood() extends SnakeGameEntities

  class SnakeGame(_width: Int, _height: Int, _food: Array[Array[Int]]) {

    println(s"w=${_width},h=${_height}")
    private val directions = Map("R" -> (1, 0), "L" -> (-1, 0), "U" -> (0, -1), "D" -> (0, 1))
    private val tail = mutable.Queue.empty[(Int, Int)]
    private val food = mutable.Queue.from(_food.map { case Array(y, x) => (y, x) })
    private val playArea = Array.fill(_height)(Array.fill(_width)(Empty().asInstanceOf[SnakeGameEntities]))

    private var length = 1
    private var x = 0
    private var y = 0
    private var gameOver = false
    private var foodAdded = false

    def move(direction: String): Int = {
      // place food
      if (!foodAdded && food.nonEmpty) {
        food.dequeue() match {
          case (fy, fx) => playArea(fy)(fx) = SnakeFood()
        }
        foodAdded = true
      }
      if (gameOver) {
        return endGame()
      }

      println(s"x=${x},y=${y}")
      println(playArea.map(_.mkString(" ")).mkString("\n"))

      directions(direction) match {
        case (offsetX, offsetY) =>
          // add tail
          tail.enqueue((y, x))
          playArea(y)(x) = SnakeTail()
          // calculate new position
          x = x + offsetX
          y = y + offsetY
          if (x < 0 || y < 0 || x >= _width || y >= _height) {
            return endGame()
          }
          val lastTail = tail.headOption
          playArea(y)(x) match {
            case Empty() =>
              moveTail()
            case SnakeTail() if lastTail.exists { case (ty, tx) => tx == x && ty == y } =>
              moveTail()
            case SnakeTail() =>
              return endGame()
            case SnakeFood() =>
              length += 1
              foodAdded = false
          }
      }
      length - 1
    }

    private def moveTail(): Unit = {
      if (tail.nonEmpty) {
        tail.dequeue() match {
          case (ty, tx) =>
            playArea(ty)(tx) = Empty()
        }
      }
    }

    private def endGame(): Int = {
      gameOver = true
      -1
    }

  }

  /**
   * Your SnakeGame object will be instantiated and called as such:
   * var obj = new SnakeGame(width, height, food)
   * var param_1 = obj.move(direction)
   */

  /**
   * Your SnakeGame object will be instantiated and called as such:
   * var obj = new SnakeGame(width, height, food)
   * var param_1 = obj.move(direction)
   */

}
