object Leet267b extends App {

  object Solution {
    def generatePalindromes(s: String): List[String] = {
      // convert string to frequency map
      val frequencyMap = s.foldLeft(Map.empty[Char, Int]) {
        case (map, char) => map.get(char) match {
          case Some(q) => map + (char -> (q + 1))
          case None => map + (char -> 1)
        }
      }
      // find the midpoint
      val midPoint = if (s.length % 2 != 0) {
        frequencyMap.collect { case (k, v) if v % 2 != 0 => k }.toList match {
          case List(a) => Some(a)
          // only one midpoint possible
          case _ => None
        }
      } else {
        None
      }
      // check if there is a solution
      if (midPoint.isEmpty && s.length % 2 != 0) return List()
      // handle 1 letter case
      if (s.length == 1) return List(s)
      smartPermute(
        totalLength = s.length,
        frequencyMap = midPoint match {
          // if there is a mid point remove it from permutation map
          case Some(c) => decrementFrequencyOrRemove(frequencyMap, key = c, fallback = Map())
          case None => frequencyMap
        },
        midPoint = midPoint,
      )
    .map(_.mkString)
    }

    def decrementFrequencyOrRemove(frequencyMap: Map[Char, Int], key: Char, fallback: Map[Char, Int]): Map[Char, Int] = {
      frequencyMap.get(key) match {
        // remove empty keys
        case Some(v) if v == 1 => frequencyMap - key
        // decrement usage
        case Some(v) if v > 1 => frequencyMap + (key -> (v - 1))
        // key not found
        case _ => fallback
      }
    }

    def smartPermute(totalLength: Int, frequencyMap: Map[Char, Int], prefix: List[Char] = List(), midPoint: Option[Char]): List[List[Char]] = {
      frequencyMap.foldLeft((List.empty[List[Char]], false)) {
        case ((aggr, stop), (k, v)) if v > 0 && totalLength == 1 && !stop => (List(List(k)) ++ aggr, false)
        // permute first half of the word
        case ((aggr, stop), (k, v)) if v > 1 && prefix.length < totalLength / 2 && !stop =>
          (smartPermute(
            totalLength = totalLength,
            frequencyMap = decrementFrequencyOrRemove(frequencyMap = frequencyMap, key = k, fallback = Map()),
            prefix = k :: prefix,
            midPoint = midPoint)
            .map(k :: _) ++ aggr
            , false)
        // mirror the second part
        case ((aggr, stop), (_, v)) if v > 0 && prefix.length >= totalLength / 2 && !stop =>
          // mirror the left part to the right
          // drop the midpoint for odd numbers
          (prefix.foldRight((frequencyMap, List.empty[Char])) {
            case (k, (frequencyMap, word)) => (decrementFrequencyOrRemove(frequencyMap = frequencyMap, key = k, fallback = Map(' ' -> 0)), k :: word)
          } match {
            // if we were able to build the right part using left chars build a word
            case (frequencyMap, word) if frequencyMap.isEmpty =>
              midPoint match {
                case Some(c) => List(c :: word) ++ aggr
                case None => List(word) ++ aggr
              }
            case _ => aggr
          }, true) // set stop flag to true to avoid duplicates, there is only 1 possible solution for each prefix
        case ((aggr, stop), _) => (aggr, stop)
      }
    }._1
  }

  //println(Solution.generatePalindromes("a"))
  //println(Solution.generatePalindromes("abc"))
  //println(Solution.generatePalindromes("aabbcc"))
  //println(Solution.generatePalindromes("aabdbcc"))
  //println(Solution.generatePalindromes("aabbhijkkjih"))
  println(Solution.generatePalindromes("aaaabbbb"))
  //println(Solution.generatePalindromes("aabbccddeeffgghh"))


}
