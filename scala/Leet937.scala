object Leet937 extends App {

  object Solution {
    def reorderLogFiles(logs: Array[String]): Array[String] = {
      logs
        .map(LogEntry.fromRawLog)
        .sortWith {
          case (LogEntry(id1, words1, isDigit1, _), LogEntry(id2, words2, isDigit2, _)) =>
            if (!isDigit1 && isDigit2) {
              true
            } else if (isDigit1 && !isDigit2) {
              false
            } else if (isDigit1 && isDigit2) {
              false
            } else if (words1 == words2) {
              id1 < id2
            } else {
              words1 < words2
            }
        }
        .map(_.raw)
    }

    case class LogEntry(id: String, content: String, isDigit: Boolean, raw: String)

    object LogEntry {
      def fromRawLog(raw: String): LogEntry = {
        raw.split(" ").toList match {
          case id :: rest =>
            val content = rest.mkString(" ")
            LogEntry(id, content, content.forall(c => c.isDigit || c == ' '), raw)
          case _ => throw new Exception("invalid log")
        }
      }
    }
  }

}
