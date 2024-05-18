object Leet224 extends App {
  trait ASTExp {
    def evaluate: Int
  }

  case class ASTScope(expression: ASTExp) extends ASTExp {
    override def evaluate: Int = expression.evaluate
  }

  case class ASTMinus(expression: ASTExp) extends ASTExp {
    override def evaluate: Int = -1 * expression.evaluate
  }

  case class ASTConst(str: String) extends ASTExp {
    override def evaluate: Int = {
      str.trim match {
        case "" => 0
        case s => s.toInt
      }
    }
  }

  case class ASTNode(head: Char, left: ASTExp, right: ASTExp) extends ASTExp {
    override def evaluate: Int = {
      head match {
        case '-' =>
          right match {
            case ASTNode(o, l, r) => left.evaluate + ASTNode(o, ASTMinus(l), r).evaluate
            case _ => left.evaluate - right.evaluate
          }
        case '+' => left.evaluate + right.evaluate
      }
    }
  }

  object Solution {
    def calculate(s: String): Int = {
      parse(s).evaluate
    }

    private def parse(s: String): ASTExp = {
      var pointer = 0
      var operand: Option[ASTExp] = None
      while (pointer < s.length) {
        s(pointer) match {
          // operator
          case Operator(o) =>
            val buf = s.splitAt(pointer)
            // using constant or scope if exists
            val op = operand.getOrElse(ASTConst(buf._1.trim))
            return ASTNode(o, op, parse(buf._2.substring(1).trim))
          // scope
          case c if c == '(' =>
            val buf = s.splitAt(pointer)
            // extract up to ) buf._2 starts with (
            val subExpression = buf._2.slice(1, lengthOfSubExpression(buf._2))
            operand = Some(ASTScope(parse(subExpression)))
            pointer += (subExpression.length + 2)
          // other
          case c =>
            pointer += 1
        }
      }
      operand.getOrElse(ASTConst(s.trim))
    }

    private def lengthOfSubExpression(s: String): Int = {
      s.foldLeft((0, 0)) {
        case ((opened, count), c) if c == '(' => (opened + 1, count + 1)
        case ((opened, count), c) if c == ')' =>
          if (opened == 1) return count
          (opened - 1, count + 1)
        case ((opened, count), c) => (opened, count + 1)
      }._2
    }

  }

  object Operator {
    val knownOps: Set[Char] = Set('+', '-')

    def unapply(char: Char): Option[Char] = {
      if (knownOps.contains(char)) {
        Some(char)
      } else None
    }
  }

  println(Solution.calculate("1 + (1 - (2 + 3) - 1)")) //-4
  println(Solution.calculate(" 2-1 + 2 ")) //3
}
