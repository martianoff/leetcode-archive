object Leet408 extends App {
  object Solution {
    def extractNumber(list: List[Char]): List[Char] = {
      list match {
        case h :: t if h >= '0' && h <= '9' =>
          h :: extractNumber(t)
        case l => List()
      }
    }

    def validWordAbbreviation(word: String, abbr: String): Boolean = {
      Iterator.unfold(word.toList, abbr.toList) {
        case (Nil, Nil) => None
        // word or abbr don't match
        case (Nil, _) => Some(false, (Nil, Nil))
        case (_, Nil) => Some(false, (Nil, Nil))
        // handle normal chars
        case (wordHead :: wordTail, c :: restAbbr) if c == wordHead => Some(true, (wordTail, restAbbr))
        // leading zeroes are not allowed
        case (_, c :: _) if c == '0' => Some(false, (Nil, Nil))
        // handle chars if doesn't match
        case (_, c :: _) if c >= 'a' && c <= 'z' => Some(false, (Nil, Nil))
        // handle numbers
        case (word, abbr) =>
          val n = extractNumber(abbr)
          val nInt = n.mkString.toInt
          if (word.size < nInt) {
            Some(false, (Nil, Nil))
          } else {
            Some(true, (word.takeRight(word.size - nInt), abbr.takeRight(abbr.size - n.size)))
          }
      }.reduceLeft(_ && _)
    }
  }
}