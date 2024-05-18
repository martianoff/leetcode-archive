import scala.collection.mutable

object Leet150 extends App {

  case class Expression(op1: Int, op2: Int, operator: Char) {
    def apply(): Int = {
      operator match {
        case '+' => op1 + op2
        case '-' => op1 - op2
        case '*' => op1 * op2
        case '/' => (op1 - (op1 % op2)) / op2
      }
    }
  }

  object Solution {
    def evalRPN(tokens: Array[String]): Int = {
      val stack = new mutable.Stack[String]()
      tokens.foreach {
        case Operator(o) =>
          stack.push(
            Expression(op2 = stack.pop().toInt, op1 = stack.pop().toInt, operator = o)().toString
          )
        case v =>
          stack.push(v)
      }
      stack.pop().toInt
    }
  }

  object Operator {
    val allowedChars: Set[Char] = Set('+', '-', '*', '/')

    def unapply(op: String): Option[Char] = {
      if (op.length == 1 && allowedChars.contains(op(0))) {
        Some(op(0))
      }
      else {
        None
      }
    }
  }

  println(Solution.evalRPN(Array("4", "13", "5", "/", "+")))
}
