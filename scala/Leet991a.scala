object Leet991a extends App {

  object Solution {
    def brokenCalc(startValue: Int, target: Int): Int = {
      var result = 0
      var t = target
      while (t > startValue) {
        result += 1
        if (t % 2 == 1) {
          t += 1
        } else {
          t /= 2
        }
      }
      result + startValue - t
    }
  }

}
