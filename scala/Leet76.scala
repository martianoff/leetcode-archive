object Leet76 extends App {

  import scala.collection.mutable

  object Solution {
    def minWindow(s: String, t: String): String = {
      var pointer1 = 0
      var pointer2 = -1
      val matches = mutable.Map.empty[Char, Int].withDefaultValue(0)
      val shouldBe = mutable.Map.empty[Char, Int].withDefaultValue(0)
      var missing = 0
      t.foreach {
        c =>
          shouldBe(c) += 1
          missing += 1
      }
      var resultStart = Option.empty[Int]
      var resultEnd = Option.empty[Int]
      while (pointer1 < s.length && ((pointer2 < s.length && missing == 0) || (pointer2 < s.length - 1 && missing > 0))) {
        // if we have missing move the right border
        if (missing > 0) {
          pointer2 += 1
          val char = s(pointer2)
          if (shouldBe.contains(char)) {
            matches(char) += 1
            // filled missing
            if (matches(char) <= shouldBe(char)) {
              missing -= 1
            }
          }
        } else {
          // otherwise move the left border
          if (missing == 0) {
            (resultStart, resultEnd) match {
              case (Some(s), Some(e)) if (e - s) > (pointer2 - pointer1) =>
                resultStart = Some(pointer1)
                resultEnd = Some(pointer2)
              case (None, None) =>
                resultStart = Some(pointer1)
                resultEnd = Some(pointer2)
              case _ =>
            }
          }
          val char = s(pointer1)
          pointer1 += 1
          if (shouldBe.contains(char)) {
            matches(char) -= 1
            // filled missing
            if (matches(char) < shouldBe(char)) {
              missing += 1
            }
          }
        }
      }
      (resultStart, resultEnd) match {
        case (Some(start), Some(end)) => s.substring(start, end + 1)
        case _ => ""
      }
    }
  }
}
