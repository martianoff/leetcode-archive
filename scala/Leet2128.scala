object Leet2128 extends App {
  object Solution {
    def removeOnes(grid: Array[Array[Int]]): Boolean = {
      grid.foldLeft((true, Option.empty[Patterns])) {
        // return true if a line matches target pattern
        case ((result, Some(patterns)), line) if result && patterns.contains(line) => (true, Some(patterns))
        // if any line doesn't match return false
        case ((_, Some(patterns)), _) => (false, Some(patterns))
        // fill out initial target patterns
        case ((result, None), line) => (result, Some(Patterns(line)))
      }._1
    }

    case class Patterns(private val direct: Array[Int]) {
      private val patterns = Set(direct.mkString, direct.map(1 - _).mkString)

      def contains(line: Array[Int]): Boolean = {
        patterns.contains(line.mkString)
      }
    }
  }

  println(Solution.removeOnes(Array(Array(0, 1, 0), Array(1, 0, 1), Array(0, 1, 0))))
}
