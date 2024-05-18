

object Leet1523 extends App {

  object Solution {
    def countOdds(low: Int, high: Int): Int = {
      1 + ((prevOdd(high) - nextOdd(low)) / 2)
    }

    def nextOdd(num: Int) = {
      if (num % 2 == 0) {
        num + 1
      } else {
        num
      }
    }

    def prevOdd(num: Int) = {
      if (num % 2 == 0) {
        num - 1
      } else {
        num
      }
    }
  }

}
