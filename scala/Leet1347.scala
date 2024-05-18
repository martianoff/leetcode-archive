import scala.collection.mutable

object Leet1347 extends App {

  object Solution {
    def minSteps(s: String, t: String): Int = {
      val diff = mutable.Map.empty[Char, Int].withDefaultValue(0)
      (0 until (s.length max t.length)).foreach {
        index =>
          // count S chars
          diff(s(index)) += 1
          // count T chars
          diff(t(index)) -= 1
      }
      diff.map { case (_, v) => v.abs }.sum / 2
    }
  }

  assert(Solution.minSteps("babasdasssaa", "abafbbssqqww") == 6)

}
