import scala.collection.mutable

object Leet505 {

  import scala.collection.mutable

  object Solution {

    def shortestDistance(maze: Array[Array[Int]], start: Array[Int], destination: Array[Int]): Int = {
      val queue = mutable.Queue.empty[(Position, Int)]
      val visited = mutable.Map.empty[Position, Int]
      val directions = List(Array(-1, 0), Array(1, 0), Array(0, -1), Array(0, 1))
      val destinationPos = Position(destination(0), destination(1))
      val startPos = Position(start(0), start(1))
      var minSteps: Option[Int] = None
      queue.enqueue((startPos, 0))
      while (queue.nonEmpty) {
        queue.dequeue() match {
          // only check if not cached or doesn't improve
          case (currentPosition, steps) if !visited.get(currentPosition).exists(_ < steps) =>
            visited(currentPosition) = steps
            if (currentPosition == destinationPos) {
              minSteps = minSteps match {
                case Some(prevMin) => Some(steps min prevMin)
                case _ => Some(steps)
              }
            } else {
              directions.foreach {
                case Array(offsetY, offsetX) =>
                  var y = currentPosition.y
                  var x = currentPosition.x
                  // move to the end
                  while (
                    x + offsetX >= 0 &&
                      y + offsetY >= 0 &&
                      y + offsetY < maze.length &&
                      x + offsetX < maze(y + offsetY).length &&
                      maze(y + offsetY)(x + offsetX) == 0
                  ) {
                    y += offsetY
                    x += offsetX
                  }
                  val potentialPos = Position(y, x)
                  val potentialSteps = steps + (x - currentPosition.x).abs + (y - currentPosition.y).abs
                  if (potentialPos != currentPosition &&
                    !visited.get(potentialPos).exists(_ < potentialSteps)) {
                    queue.enqueue((
                      potentialPos,
                      potentialSteps
                    ))
                  }
              }
            }
          case _ =>
        }
      }
      minSteps.getOrElse(-1)
    }

    case class Position(y: Int, x: Int)
  }

}