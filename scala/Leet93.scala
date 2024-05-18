object Leet93 extends App {
  object Solution {
    def restoreIpAddresses(s: String): List[String] = {
      backtrack(s.toList, 4)
        .collect {
          case Some(v) => v.mkString(".")
        }
    }

    def validOptions(options: List[Option[List[String]]], requiredParts: Int): List[List[String]] = {
      options
        .collect { case Some(v) if v.size == requiredParts => v }
    }

    def backtrack(s: List[Char], parts: Int): List[Option[List[String]]] = {
      if (parts == 0) {
        if (s.nonEmpty)
          List(None)
        else
          List(Some(List.empty[String]))
      } else s match {
        case Nil => List(Some(List.empty[String]))
        // 0 can be separate entity only
        case h :: t if h == '0' =>
          validOptions(backtrack(t, parts - 1), parts - 1)
            .map(h.toString :: _)
            .map(Some(_))
        case h :: t =>
          Seq[Option[List[Option[List[String]]]]](
            // explore 3 length path
            t match {
              case a :: b :: t if (h.toString + a.toString + b.toString).toInt <= 255 =>
                Some(validOptions(backtrack(t, parts - 1), parts - 1)
                  .map((h.toString + a.toString + b.toString) :: _)
                  .map(Some(_)))
              case _ => None
            },
            // explore 2 length path
            t match {
              case a :: t =>
                Some(validOptions(backtrack(t, parts - 1), parts - 1)
                  .map((h.toString + a.toString) :: _)
                  .map(Some(_)))
              case _ => None
            },
            // explore 1 length path
            Some(validOptions(backtrack(t, parts - 1), parts - 1)
              .map(h.toString :: _)
              .map(Some(_)))
          )
            .collect { case Some(v) => v }
            .reduceLeft(_ ++ _)
      }
    }
  }

  println(Solution.restoreIpAddresses("0000"))
  println(Solution.restoreIpAddresses("25525511135"))
  println(Solution.restoreIpAddresses("101023"))
}
