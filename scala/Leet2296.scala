object Leet2296 extends App {

  class TextEditor() {

    private var left = Vector[Char]()
    private var right = Vector[Char]()

    def addText(text: String) {
      left = text.foldLeft(left) {
        case (left, char) => left.appended(char)
      }
    }

    def deleteText(k: Int): Int = {
      val numCharsToRemove = (k min left.length)
      left = (0 until numCharsToRemove).foldLeft(left) {
        case (buf, _) => buf.init
      }
      numCharsToRemove
    }

    def cursorLeft(k: Int): String = {
      // move chars from left's end to beggining of right
      (0 until (k min left.length)).foreach {
        _ =>
          right = right.prepended(left.last)
          left = left.init
      }
      lastTen()
    }

    def cursorRight(k: Int): String = {
      // move chars from rights's beggining to left's end
      (0 until (k min right.length)).foreach {
        _ =>
          left = left.appended(right.head)
          right = right.tail
      }
      lastTen()
    }

    private def lastTen(): String = {
      (0 until (10 min left.length)).foldLeft((List.empty[Char], left)) {
        case ((list, buf), _) =>
          (buf.last :: list, buf.init)
      }._1.mkString
    }

  }

}
