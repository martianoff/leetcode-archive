object Leet276a extends App {

  import scala.util.chaining._

  object Solution {
    def numWays(n: Int, k: Int): Int = {
      paintCombo(currentIndex = 0, prevColor = -1, colorLength = 0, size = n, colorsCount = k)
        .tap(println)
        .length
    }

    def paintCombo(currentIndex: Int, prevColor: Int, colorLength: Int, size: Int, colorsCount: Int): List[List[Int]] = {
      if (currentIndex >= size) {
        return List(List.empty[Int])
      }
      (0 until colorsCount).collect {
        case newColor if newColor != prevColor =>
          paintCombo(
            currentIndex = currentIndex + 1,
            prevColor = newColor,
            colorLength = 1,
            size = size,
            colorsCount = colorsCount).map {
            l => newColor :: l
          }
        case newColor if newColor == prevColor && colorLength == 1 =>
          paintCombo(
            currentIndex = currentIndex + 1,
            prevColor = newColor,
            colorLength = 2,
            size = size,
            colorsCount = colorsCount).map {
            l => newColor :: l
          }
      }.flatten.toList
    }
  }

}
