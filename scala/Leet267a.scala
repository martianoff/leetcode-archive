object Leet267a extends App {

  object Solution {
    def generatePalindromes(s: String): List[String] = {
      smartPermute(
        totalLength = s.length,
        // convert string to frequency map
        frequencyMap = s.foldLeft(Map.empty[Char, Int]) {
          case (map, char) => map.get(char) match {
            case Some(q) => map + (char -> (q + 1))
            case None => map + (char -> 1)
          }
        }
      )
        .map(_.mkString)
    }

    def smartPermute(totalLength: Int, frequencyMap: Map[Char, Int], prefix: List[Char] = List()): List[List[Char]] = {
      frequencyMap.foldLeft((List.empty[List[Char]], false)) {
        case ((aggr, stop), (k, v)) if v > 0 && totalLength == 1 && !stop => (List(List(k)) ++ aggr, false)
        // permute first half of the word
        case ((aggr, stop), (k, v)) if v > 0 && prefix.length < totalLength / 2 + (if (totalLength % 2 != 0) 1 else 0) && !stop =>
          (smartPermute(
            totalLength = totalLength,
            frequencyMap = frequencyMap.get(k) match {
              // remove empty keys
              case Some(v) if v == 1 => frequencyMap - k
              // decrement usage
              case Some(v) if v > 1 => frequencyMap + (k -> (v - 1))
              case _ => Map.empty[Char, Int]
            },
            prefix = k :: prefix)
            .map(k :: _) ++ aggr
            , false)
        // mirror the second part
        case ((aggr, stop), (_, v)) if v > 0 && !stop =>
          // mirror the left part to the right
          // drop the midpoint for odd numbers
          (prefix.drop(if (totalLength % 2 != 0) 1 else 0).foldRight((frequencyMap, List.empty[Char])) {
            case (k, (frequencyMap, word)) => frequencyMap.get(k) match {
              case Some(v) if v > 0 => (frequencyMap + (k -> (v - 1)), k :: word)
              case _ => (frequencyMap, word)
            }
          } match {
            // if we were able to build the right part using left chars build a word
            case (frequencyMap, word) if !frequencyMap.exists(_._2 > 0) =>
              List(word) ++ aggr
            case _ => aggr
          }, true) // set stop flag to true to avoid duplicates, there is only 1 possible solution for each prefix
        case ((aggr, stop), _) => (aggr, stop)
      }
    }._1
  }

  //println(Solution.generatePalindromes("a"))
  //println(Solution.generatePalindromes("abc"))
  println(Solution.generatePalindromes("aabbcc"))
  //println(Solution.generatePalindromes("aabdbcc"))
  //println(Solution.generatePalindromes("aabbhijkkjih"))
  //println(Solution.generatePalindromes("aabbccddeeffgghh"))


}
