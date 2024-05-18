object Leet49 extends App {
  object Solution {
    def groupAnagrams(strs: Array[String]): List[List[String]] = {
      strs.map(v => (v, hash(v))).groupBy(_._2).map {
        case (k, v) => v.map(_._1).toList
      }.toList
    }

    def hash(s: String) = {
      s.foldLeft(0, 1, 1) {
        case ((lhash, mhash, rhash), char) =>
          (lhash + (char - 'a'), mhash * ('z' - char + 1), rhash * (char - 'a' + 1))
      } match {
        case (lhash, mhash, rhash) => s.length + "#" + lhash + "#" + mhash + "#" + rhash
      }
    }
  }
}
