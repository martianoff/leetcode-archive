object Leet77 extends App {
  object Solution {
    def combine(n: Int, k: Int): List[List[Int]] = {
      combineOptions((1 to n).toSet, k)
    }

    def combineOptions(options: Set[Int], k: Int): List[List[Int]] = {
      if (k == 0) List(List())
      else {
        options.toList.flatMap {
          v =>
            combineOptions(options - v, k - 1).collect {
              // deduplicate by building increasing sequences only
              case head :: tail if v < head => v :: head :: tail
              case Nil => List(v)
            }
        }
      }
    }
  }
  //println(Solution.combine(13,13))
  println(Solution.combine(4, 2))
  println(Solution.combine(4, 3))
  println(Solution.combine(4, 4))

}
