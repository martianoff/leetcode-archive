object Leet44 extends App {

  import scala.util.chaining._
  import scala.collection.mutable

  object Solution {
    def isMatch(s: String, p: String): Boolean = {
      val cache = mutable.Map.empty[(Int, Int), Boolean]
      search(cache, s, p.zipWithIndex.foldRight(List.empty[Char]) {
        case ((c, i), list) if c != '*' => c :: list
        case ((c, i), list) if i >= p.length - 1 => c :: list
        case ((c, i), list) if c == '*' && p(i + 1) != '*' => c :: list
        // fold duplicated stars into single star
        case (_, list) => list
      }.pipe(_.mkString))
    }

    private def search(cache: mutable.Map[(Int, Int), Boolean], s: String, p: String, pointerS: Int = 0, pointerP: Int = 0): Boolean = {
      if (cache.contains((pointerS, pointerP))) {
        return cache((pointerS, pointerP))
      }
      if (pointerS == s.length) {
        // if pattern is not completely matched,
        // we need to check if remaining pattern contains something other than *
        (pointerP until p.length).foreach {
          i =>
            if (p(i) != '*') {
              return false
            }
        }
        return true
      }
      if (pointerP == p.length) {
        return false
      }
      (p(pointerP) match {
        case '*' =>
          // * transformed to long sequence
          search(cache, s, p, pointerS + 1, pointerP) ||
            // * transformed to empty sequence
            search(cache, s, p, pointerS, pointerP + 1)
        case '?' =>
          search(cache, s, p, pointerS + 1, pointerP + 1)
        case c if s(pointerS) == c =>
          search(cache, s, p, pointerS + 1, pointerP + 1)
        case _ =>
          return false
      }).tap {
        r => cache += ((pointerS, pointerP) -> r)
      }
    }
  }
}
