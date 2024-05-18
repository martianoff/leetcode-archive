object Leet1861 extends App {
  object Solution {
    def rotateTheBox(box: Array[Array[Char]]): Array[Array[Char]] = {
      val originalHeight = box.length
      val originalWidth = box(0).length
      // empty rotated box
      val rotatedBox = Array.fill(originalWidth)(Array.fill(originalHeight)('.'))
      // rotate original box
      box.zipWithIndex.foreach {
        case (row, y) =>
          row.zipWithIndex.foreach {
            case (value, x) =>
              rotatedBox(x)(originalHeight - y - 1) = value
          }
      }
      // apply gravity
      (0 until originalHeight).foreach {
        x =>
          var stones = 0
          (0 until originalWidth).foreach {
            y =>
              rotatedBox(y)(x) match {
                case '#' =>
                  // remove stones and count them
                  stones += 1
                  rotatedBox(y)(x) = '.'
                case '*' =>
                  // release stones on top of the obstacle
                  addStonesOnTopOf(rotatedBox, stones, x, y)
                  stones = 0
                case _ =>
              }
          }
          // emulate obstacles in the bottom
          addStonesOnTopOf(rotatedBox, stones, x, originalWidth)
      }
      rotatedBox
    }

    private def addStonesOnTopOf(box: Array[Array[Char]], numberOfStones: Int, x: Int, y: Int): Unit = {
      (1 to numberOfStones).foreach {
        offset => box(y - offset)(x) = '#'
      }
    }
  }
}
