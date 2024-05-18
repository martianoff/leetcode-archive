object Leet77c extends App {
  object Solution {
    def combine(n: Int, k: Int): List[List[Int]] = {
      combineOptions((1 to n).toList, k)
    }

    def combineOptions(options: List[Int], k: Int): List[List[Int]] = {
      if (k == 0) List(List.empty[Int]) // empty value is required for "::" operation
      else {
        options match {
          case Nil => List.empty[List[Int]]
          case h :: t =>
            // combinations starting with this number
            combineOptions(t, k - 1).map(h :: _) ++
              // combinations starting with other numbers
              combineOptions(t, k)
        }
      }
    }
  }

  println(Solution.combine(1, 1))
  println(Solution.combine(13, 13))
  println(Solution.combine(4, 2))
  println(Solution.combine(4, 3))
  println(Solution.combine(4, 4))

}
