object Leet12a extends App {

  import scala.collection.immutable

  object Solution {
    val conversionMap: immutable.TreeMap[Int, String] = immutable.TreeMap(
      1 -> "I", 4 -> "IV", 5 -> "V", 9 -> "IX", 10 -> "X", 40 -> "XL", 50 -> "L",
      90 -> "XC", 100 -> "C", 400 -> "CD", 500 -> "D", 900 -> "CM", 1000 -> "M"
    )

    def intToRoman(num: Int): String = {
      build(num).mkString
    }

    private def build(num: Int): List[String] = {
      conversionMap.rangeTo(num).last match {
        case (k, v) if num - k > 0 => v :: build(num - k)
        case (_, v) => List(v)
      }
    }
  }

  println(Solution.intToRoman(323))
}
