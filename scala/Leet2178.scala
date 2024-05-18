import scala.collection.mutable

object Leet2178 extends App {
  object Solution {
    def maximumEvenSplit(finalSum: Long): List[Long] = {
      // the sum can't be non even
      if (finalSum % 2 != 0) return List()
      // find even numbers that in sum gives us ta number more than finalSum
      val set = mutable.SortedSet.from(List.unfold(2L, 2L) {
        case (evenNumber, sumOfEvenNumbers) if sumOfEvenNumbers > finalSum => None
        case (evenNumber, sumOfEvenNumbers) => Some(evenNumber, (evenNumber + 2, sumOfEvenNumbers + evenNumber))
      })
      var setSum = set.sum
      while (setSum - finalSum > 0) {
        // remove the closest number to the difference between target and setSum
        set.rangeFrom(setSum - finalSum).headOption match {
          case Some(closest) =>
            set -= closest
            setSum -= closest
          case _ =>
            // not possible
            setSum = finalSum
            set.clear
        }
      }
      set.toList
    }
  }
}
