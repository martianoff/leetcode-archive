object Leet251 extends App {

  import scala.util.chaining._

  class Vector2D(_vec: Array[Array[Int]]) {

    private val height = _vec.length
    private var row = 0
    private var col = 0
    private var more = height > 0

    def next(): Int = {
      _vec(row)(col).tap {
        _ =>
          advancePointer()
          scrollToNonEmptyRow()
      }
    }

    def hasNext(): Boolean = {
      more
    }

    private def widthOfCurrentRow: Int = _vec(row).length

    scrollToNonEmptyRow()

    private def advancePointer(): Unit = {
      if (col >= widthOfCurrentRow - 1) {
        col = 0
        if (row < height - 1) {
          row += 1
        } else {
          more = false
        }
      } else {
        col += 1
      }
    }

    private def scrollToNonEmptyRow(): Unit = {
      // skip empty rows
      while (more && _vec(row).length == 0) {
        advancePointer()
      }
    }

  }
}
