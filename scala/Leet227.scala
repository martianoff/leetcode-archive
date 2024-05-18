object Leet227 extends App {
  object Solution {
    def calculate(s: String): Int = {
      // simplify task by removing empty spaces and adding "=" on the end
      ((s.replace(" ", "") + "=")
        // convert string to list of expressions
        .foldLeft((LongExpression(ops = List()), List.empty[String])) {
          case ((exp, buf), char) if char != '*' && char != '/' && char != '+' && char != '-' && char != '=' =>
            (exp, char.toString :: buf)
          case ((LongExpression(ops), buf), char) if char == '=' =>
            (LongExpression(SimpleExpression(buf.reverse.mkString) :: ops), List())
          // met an operator and operand is in buffer
          case ((LongExpression(ops), buf), char) if ops.isEmpty =>
            (LongExpression(Operator(char) :: SimpleExpression(buf.reverse.mkString) :: ops), List())
          // met an operator and first operand is filled, second is in buffer
          case ((LongExpression(oldOperator :: opstail), buf), char) if opstail.nonEmpty =>
            (LongExpression(Operator(char) :: SimpleExpression(buf.reverse.mkString) :: oldOperator :: opstail), List())
          // ignore everything else
          case ((exp, buf), _) => (exp, buf)
        }._1 match {
        // list is reversed
        case LongExpression(reverseOps) => LongExpression(reverseOps.reverse)
      }).evaluate()
    }

    trait Expression {
      def evaluate(): Int
    }

    case class Operator(char: Char) extends Expression {
      def evaluate(): Int = 0
    }

    // evaluate simple expressions
    case class SimpleExpression[T](value: T) extends Expression {
      def evaluate(): Int = value match {
        case i: Int => i
        case s: String => s.toInt
      }
    }

    // evaluate a list of any expressions
    case class LongExpression(ops: List[Expression]) extends Expression {
      def evaluate(): Int = {
        ops match {
          case List(a: SimpleExpression[_]) => a.evaluate()
          case op1 :: operator :: op2 :: otherExpressions =>
            operator match {
              // priority on * and /
              case Operator('*') => LongExpression(SimpleExpression(op1.evaluate() * op2.evaluate()) :: otherExpressions).evaluate()
              case Operator('/') => LongExpression(SimpleExpression(op1.evaluate() / op2.evaluate()) :: otherExpressions).evaluate()
              case Operator('+') => SimpleExpression(op1.evaluate() + LongExpression(op2 :: otherExpressions).evaluate()).evaluate()
              case Operator('-') => SimpleExpression(op1.evaluate() + LongExpression(SimpleExpression(-1) :: Operator('*') :: op2 :: otherExpressions).evaluate()).evaluate()
            }
          case _ => 0
        }
      }
    }
  }
  //println(Solution.calculate("1+2+3"))
  //println(Solution.calculate("1+2-3"))
  //println(Solution.calculate("1-2*3"))
  //println(Solution.calculate(" 3+5 / 2 "))
  //println(Solution.calculate("1-1+1"))
  println(Solution.calculate("1*2+3*4"))
}
