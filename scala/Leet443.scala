object Leet443 extends App {
  object Solution {
    def compress(chars: Array[Char]): Int = {
      var groupChar = Option.empty[Char]
      var groupLength = 0
      var groupWriterPosition = 0
      chars.zipWithIndex.foreach {
        case (c, i) =>
          groupChar match {
            // populate opened group
            case Some(gc) if gc == c =>
              groupLength += 1
            // write sequence and start a new group
            case Some(gc) =>
              groupWriterPosition = writeToBuffer(chars = chars,
                groupChar = gc,
                groupLength = groupLength,
                writeAt = groupWriterPosition)
              groupChar = Some(c)
              groupLength = 1
            // initial group
            case None =>
              groupChar = Some(c)
              groupLength = 1
          }
      }
      // close the last open group
      groupChar match {
        case Some(gc) =>
          groupWriterPosition = writeToBuffer(chars = chars,
            groupChar = gc,
            groupLength = groupLength,
            writeAt = groupWriterPosition)
        case _ =>
      }
      groupWriterPosition
    }

    def writeToBuffer(chars: Array[Char], groupChar: Char, groupLength: Int, writeAt: Int): Int = {
      chars(writeAt) = groupChar
      // if group longer than 1 wrap length
      if (groupLength > 1) {
        groupLength.toString.zipWithIndex.foreach {
          case (nc, ni) =>
            chars(writeAt + ni + 1) = nc
        }
        writeAt + 1 + groupLength.toString.length
      } else {
        writeAt + 1
      }
    }
  }
}
