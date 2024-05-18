object Leet755b extends App {

  object Solution {
    def pourWater(heights: Array[Int], volume: Int, k: Int): Array[Int] = {
      var x = k
      (0 until volume).foreach { _ =>
        var y = heights(x)
        // move left adjusting y
        while (x > 0 && heights(x - 1) <= y) {
          y = heights(x - 1)
          x -= 1
        }
        // move right adjusting y
        while (x + 1 < heights.length && heights(x + 1) <= y) {
          y = heights(x + 1)
          x += 1
        }
        // move left to x = k within the same y
        while (x > k && heights(x - 1) <= y) {
          x -= 1
        }
        heights(x) += 1
      }
      heights
    }
  }

  println(List(1, 2, 3).init)
  println(List(1, 2, 3).tail)

}
