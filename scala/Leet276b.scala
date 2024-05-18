object Leet276b extends App {

  object Solution {
    def numWays(n: Int, k: Int): Int = {
      if (n == 1) return k
      if (n == 2) return k * k
      paintCombo(currentIndex = 0, prevColor = -1, colorLength = 0, size = n, colorsCount = k)
    }

    def paintCombo(currentIndex: Int, prevColor: Int, colorLength: Int, size: Int, colorsCount: Int): Int = {
      if (currentIndex >= size) {
        return 1
      }
      (0 until colorsCount).collect {
        case newColor if newColor != prevColor =>
          paintCombo(
            currentIndex = currentIndex + 1,
            prevColor = newColor,
            colorLength = 1,
            size = size,
            colorsCount = colorsCount)
        case newColor if newColor == prevColor && colorLength == 1 =>
          paintCombo(
            currentIndex = currentIndex + 1,
            prevColor = newColor,
            colorLength = 2,
            size = size,
            colorsCount = colorsCount)
      }.sum
    }
  }

}
