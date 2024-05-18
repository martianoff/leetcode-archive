object Leet777 extends App {

  object Solution {
    def canTransform(start: String, end: String): Boolean = {
      (lrPos(start), lrPos(end)) match {
        // check corner cases
        case (startLR, endLR) if startLR.size != endLR.size => false
        // check main case
        case (startLR, endLR) =>
          startLR.foldLeft((endLR, true)) {
            // exit as soon as we get false
            case ((_, result), _) if !result => (List(), result)
            // scroll target in parallel with start
            // check if we have the same order of L-R because they can't jump over each other
            // check that target L located on the left side
            // check that target R located on the right side
            case ((('R', endPosCur) :: endPosRest, result), ('R', charPos)) if endPosCur >= charPos =>
              (endPosRest, result)
            case ((('L', endPosCur) :: endPosRest, result), ('L', charPos)) if endPosCur <= charPos =>
              (endPosRest, result)
            // not possible to build
            case (_, _) => (List(), false)
          }._2
      }
    }

    // remove all 'X' and save original position of all 'L/R' instead
    def lrPos(s: String): List[(Char, Int)] = {
      s.toList.zipWithIndex.collect {
        case (c, i) if c == 'R' || c == 'L' => (c, i)
      }
    }
  }
}
