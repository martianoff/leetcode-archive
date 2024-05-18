object Leet97b extends App {

  import scala.collection.mutable
  import scala.util.chaining._

  object Solution {
    def isInterleave(s1: String, s2: String, s3: String): Boolean = {
      if (s1.length + s2.length - s3.length != 0) return false
      val cache = mutable.Map.empty[(List[Char], List[Char]), Boolean]
      isInterleave(s1.toList, s2.toList, s3.toList, cache)
    }

    private def isInterleave(s1: List[Char], s2: List[Char], s3: List[Char], cache: mutable.Map[(List[Char], List[Char]), Boolean]): Boolean = {
      cache.get((s1, s2)) match {
        case Some(v) => return v
        case _ =>
      }
      ((s1, s2, s3) match {
        // completely matched
        case (Nil, Nil, Nil) => true
        // matches s1
        case (s1head :: s1tail, Nil, s3head :: s3tail) if s3head == s1head =>
          isInterleave(s1tail, s2, s3tail, cache)
        case (s1head :: s1tail, s2head :: s2tail, s3head :: s3tail) if s1head != s2head && s3head == s1head =>
          isInterleave(s1tail, s2, s3tail, cache)
        // matches s2
        case (Nil, s2head :: s2tail, s3head :: s3tail) if s3head == s2head =>
          isInterleave(s1, s2tail, s3tail, cache)
        case (s1head :: s1tail, s2head :: s2tail, s3head :: s3tail) if s1head != s2head && s3head == s2head =>
          isInterleave(s1, s2tail, s3tail, cache)
        // matches both
        case (s1head :: s1tail, s2head :: s2tail, s3head :: s3tail) if s1head == s2head && s3head == s1head =>
          isInterleave(s1tail, s2, s3tail, cache) || isInterleave(s1, s2tail, s3tail, cache)
        // not possible
        case _ => false
      }).tap {
        result => cache += ((s1, s2) -> result)
      }
    }
  }
}
