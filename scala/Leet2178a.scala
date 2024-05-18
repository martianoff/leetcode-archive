object Leet2178a extends App {
  object Solution {
    def maximumEvenSplit(finalSum: Long): List[Long] = {
      // the sum can't be non even
      if (finalSum % 2 != 0) return List()
      // start with number 2 and sum of elements in the list equals 0
      List.unfold(2L, 0L) {
        // termination case
        case (evenNumber, sumOfEvenNumbers) if evenNumber == 0 => None
        // check if we can add two more elements
        case (evenNumber, sumOfEvenNumbers) if sumOfEvenNumbers + 2 * evenNumber + 2 <= finalSum =>
          Some(evenNumber, (evenNumber + 2, sumOfEvenNumbers + evenNumber))
        // else add the final element
        case (evenNumber, sumOfEvenNumbers) =>
          Some(finalSum - sumOfEvenNumbers, (0, 0))
      }
    }
  }
}
