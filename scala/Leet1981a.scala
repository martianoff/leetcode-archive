object Leet1981a extends App {

  def minimizeTheDifference(mat: Array[Array[Int]], target: Int): Int = {
    if (mat.length == 0) {
      return target
    }
    mat.map(_.min).sum match {
      // min sum greater than target
      case possibleMin if possibleMin > target => possibleMin - target
      case possibleMin =>
        // calculate all possible sums
        // sum should be <= 2 * target - possibleMin
        //
        // explanation:
        // sum < target + bestDifferenceSoFar
        // bestDifferenceSoFar = possibleMin - target
        // sum < target + (target - possibleMin)
        // sum < 2 * target - possibleMin
        mat.foldLeft(Set.empty[Int]) {
            case (possibleSums, line) if possibleSums.isEmpty => line.toSet[Int]
            case (possibleSums, line) =>
              line.toSet[Int].flatMap {
                  newValue =>
                    possibleSums.collect {
                      case oldValue if newValue + oldValue <= 2 * target + possibleMin =>
                        newValue + oldValue
                    }
                }
                .partition(_ < target) match {
                case (smaller, bigger) if bigger.isEmpty => smaller
                // for sums bigger than target we can explore only smallest sums
                // because all other options will bigger difference
                case (smaller, bigger) => smaller ++ Set(bigger.min)
              }
          }
          // possible to short it to .map(v => (target - v).abs).min
          // but it will use more memory and will lead to "Memory Limit Exceeded"
          .foldLeft(-1) {
            // start with -1 as min and replace it with any first diff
            case (min, v) if min == -1 => (target - v).abs
            // search for better option
            case (min, v) if (target - v).abs < min => (target - v).abs
            // or use old min
            case (min, v) => min
          }
    }
  }

}
