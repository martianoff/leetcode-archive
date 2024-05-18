import scala.collection.mutable
import scala.util.chaining._

object Leet1419 extends App {

  import scala.collection.mutable

  object Solution {
    def minNumberOfFrogs(croakOfFrogs: String): Int = {
      var result = 0
      var curr = 0
      var freq = mutable.Map.empty[Char, Int].withDefaultValue(0)
      croakOfFrogs.foreach {
        c =>
          freq(c) += 1
          if (!valid(freq)) {
            return -1
          }
          if (c == 'c') {
            curr += 1
          } else if (c == 'k') {
            curr -= 1
          }
          result = result max curr
      }
      if (curr > 0) {
        return -1
      }
      result
    }

    def valid(freq: mutable.Map[Char, Int]): Boolean = {
      freq('c') >= freq('r') &&
        freq('r') >= freq('o') &&
        freq('o') >= freq('a') &&
        freq('a') >= freq('k')
    }
  }

}
