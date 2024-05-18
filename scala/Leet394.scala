object Leet394 extends App {

  import scala.collection.mutable

  object Solution {
    def decodeString(s: String): String = {
      val commands = mutable.Stack.empty[Ops]
      val numBuffer = new mutable.StringBuilder()
      val buffer = new mutable.StringBuilder()
      val result = new mutable.StringBuilder()
      s.foreach {
        case char if char == '[' =>
          commands.push(Command(times = numBuffer.mkString.toInt, prefix = buffer.mkString))
          buffer.clear()
          numBuffer.clear()
        case char if char == ']' =>
          val s = buffer.mkString
          buffer.clear()
          commands.pop() match {
            case Command(times, prefix) =>
              buffer.append(prefix)
              buffer.append((0 until times).map(_ => s).mkString)
          }
          if (commands.isEmpty) {
            result.append(buffer)
            buffer.clear()
          }
        case char if char >= '0' && char <= '9' =>
          numBuffer.append(char)
        case char =>
          buffer.append(char)
      }
      result.append(buffer)
      result.mkString
    }

    // pattern a + b * c
    // s2[a3[c]]
    // s + 2 * ( a + 3 * c)
    trait Ops

    case class Command(times: Int, prefix: String) extends Ops
  }

}
