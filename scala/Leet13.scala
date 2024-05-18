object Leet13 extends App {

  object Solution {
    def romanToInt(s: String): Int = {
      val map: Map[Char, Int] = Map(
        'I' -> 1,
        'V' -> 5,
        'X' -> 10,
        'L' -> 50,
        'C' -> 100,
        'D' -> 500,
        'M' -> 1000,
      )
      s
        //convert to the list of int values
        //for example: IV => 1,5, XX => 10,10
        .map(map)
        //fold list adding negatives: if value decreases append as is, else add two more negatives
        //for example: IV => 1,5 => 1,-1*2,5 = 4
        .foldLeft(List[Int](0))((a, b) => {
          if (a.head >= b) {
            b :: a
          } else {
            b :: -2 * a.head :: a
          }
        })
        //calc sum
        .sum
    }
  }

  println(Solution.romanToInt("XX"))
  println(Solution.romanToInt("II"))
  println(Solution.romanToInt("IV"))
  println(Solution.romanToInt("LVIII")) //58
  println(Solution.romanToInt("MCMXCIV")) //1994

}
