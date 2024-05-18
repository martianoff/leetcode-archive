object Leet12 extends App {

  import scala.collection.mutable

  object Solution {
    val conversionMap: Map[Int, String] = Map(
      1 -> "I",
      4 -> "IV",
      5 -> "V",
      9 -> "IX",
      10 -> "X",
      40 -> "XL",
      50 -> "L",
      90 -> "XC",
      100 -> "C",
      400 -> "CD",
      500 -> "D",
      900 -> "CM",
      1000 -> "M"
    )
    val sortedNums: Seq[Int] =
      conversionMap.keys.toSeq.sortWith(_ > _)

    def intToRoman(num: Int): String = {
      val stack = mutable.Stack.from(sortedNums.filter(_ <= num))
      val roman = new mutable.StringBuilder()
      var target = num
      while (stack.nonEmpty) {
        stack.pop() match {
          case closestMatch =>
            while (target >= closestMatch) {
              target = target - closestMatch
              roman ++= conversionMap(closestMatch)
            }
        }
      }
      roman.toString
    }
  }


}
