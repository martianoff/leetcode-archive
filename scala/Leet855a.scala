object Leet855a extends App {

  import scala.collection.mutable

  val test = new ExamRoom(8)

  class ExamRoom(_n: Int) {

    val beginOfIntervalToInterval = mutable.TreeMap.empty[Int, Int]
    val intervalsToBeginOfInterval = mutable.TreeMap.empty[Int, mutable.TreeSet[Int]]
    val occupiedSeats = mutable.Set.empty[Int]

    // start with available interval 0 - _n
    intervalsToBeginOfInterval += (_n -> mutable.TreeSet(0))
    beginOfIntervalToInterval += (0 -> _n)

    def seat(): Int = {
      (intervalsToBeginOfInterval.lastOption match {
        // regular case - pick the longest interval
        case Some((intervalLength, intervalStarts)) =>
          // beginning of interval
          if (!occupiedSeats.contains(intervalStarts.head - 1)) {
            intervalStarts.head
            // end of interval
          } else if (!occupiedSeats.contains(intervalStarts.head + intervalLength)) {
            intervalStarts.head + intervalLength - 1
          } else {
            // there is also another possible option with longest interval - 1 and lower start index
            // we need to pick an option with the lowest start index
            val alternativeIntervalLength = intervalLength - 1
            intervalsToBeginOfInterval.get(alternativeIntervalLength) match {
              case Some(alternativeIntervalStarts) if alternativeIntervalStarts.head < intervalStarts.head && alternativeIntervalLength % 2 != 0 && alternativeIntervalLength > 0 =>
                alternativeIntervalStarts.head + ((alternativeIntervalLength + 1) / 2) - 1
              case _ if intervalLength % 2 != 0 =>
                intervalStarts.head + (intervalLength + 1) / 2 - 1
              case _ =>
                intervalStarts.head + intervalLength / 2 - 1
            }
          }
        // empty room
        case _ =>
          0
      }) match {
        // save to the storage
        case seatNumber =>
          // add new interval to the storage
          removePoint(seatNumber)
          seatNumber
      }
    }

    private def removePoint(position: Int): Unit = {
      occupiedSeats += position
      // find containing interval
      (beginOfIntervalToInterval.rangeTo(position).lastOption match {
        case Some((prevIntervalPos, prevIntervalLength)) =>
          // remove old interval
          dropInterval(prevIntervalPos, prevIntervalLength)
          List(
            (prevIntervalPos, position - prevIntervalPos),
            (position + 1, prevIntervalLength - (position - prevIntervalPos) - 1)
          )
        case _ =>
          List()
      }).foreach {
        case (start, length) =>
          appendInterval(start, length)
      }
    }

    private def dropInterval(start: Int, length: Int) = {
      // remove old interval
      beginOfIntervalToInterval -= start
      intervalsToBeginOfInterval(length) -= start
      if (intervalsToBeginOfInterval(length).isEmpty) {
        intervalsToBeginOfInterval -= length
      }
    }

    private def appendInterval(start: Int, length: Int) = {
      // add new interval
      if (length >= 1) {
        if (!intervalsToBeginOfInterval.contains(length)) {
          intervalsToBeginOfInterval(length) = mutable.TreeSet.empty[Int]
        }
        beginOfIntervalToInterval += (start -> length)
        intervalsToBeginOfInterval(length) += start
      }
    }

    def leave(p: Int): Unit = {
      addPoint(p)
    }

    private def addPoint(position: Int): Unit = {
      occupiedSeats -= position
      ((beginOfIntervalToInterval.rangeUntil(position).lastOption, beginOfIntervalToInterval.rangeFrom(position + 1).headOption) match {
        case (Some((prevIntervalPos, prevIntervalLength)), Some((nextIntervalPos, nextIntervalLength)))
          if nextIntervalPos == position + 1 && prevIntervalPos + prevIntervalLength == position =>
          // concat two intervals
          dropInterval(prevIntervalPos, prevIntervalLength)
          dropInterval(nextIntervalPos, nextIntervalLength)
          List((prevIntervalPos, prevIntervalLength + nextIntervalLength + 1))
        case (_, Some((nextIntervalPos, nextIntervalLength))) if nextIntervalPos == position + 1 =>
          dropInterval(nextIntervalPos, nextIntervalLength)
          List((position, nextIntervalLength + 1))
        case (Some((prevIntervalPos, prevIntervalLength)), _) if prevIntervalPos + prevIntervalLength == position =>
          dropInterval(prevIntervalPos, prevIntervalLength)
          List((prevIntervalPos, prevIntervalLength + 1))
        case _ => List((position, 1))
      }).foreach {
        case (start, length) =>
          appendInterval(start, length)
      }
    }

  }

  println(test.seat())
  println(test.seat())
  println(test.seat())
  println(test.leave(0))
  println(test.leave(7))
  println(test.seat())
  println(test.seat())
  println(test.seat())
  println(test.seat())
  println(test.seat())
  println(test.seat())
  println(test.seat())

  /**
   * ["ExamRoom","seat","seat","seat","seat","leave","seat"]
   * [[10],[],[],[],[],[4],[]]
   *
   * val test = new ExamRoom(10)
   * println(test.seat())
   * println(test.seat())
   * println(test.seat())
   * println(test.seat())
   * println(test.leave(4))
   * println(test.seat())
   *
   * ["ExamRoom","seat","seat","seat","seat","leave","leave","seat"]
   * [[4],[],[],[],[],[1],[3],[]]
   *
   * ["ExamRoom","seat","seat","seat","leave","leave","seat","seat","seat","seat","seat","seat","seat","seat","seat","leave"]
   * [[10],[],[],[],[0],[4],[],[],[],[],[],[],[],[],[],[0]]
   *
   * ["ExamRoom","seat","seat","seat","leave","leave","seat","seat","seat","seat","seat","seat","seat","seat","seat","leave","leave","seat","seat","leave","seat","leave","seat","leave","seat","leave","seat","leave","leave","seat","seat","leave","leave","seat","seat","leave"]
   * [[10],[],[],[],[0],[4],[],[],[],[],[],[],[],[],[],[0],[4],[],[],[7],[],[3],[],[3],[],[9],[],[0],[8],[],[],[0],[8],[],[],[2]]
   *
   * ["ExamRoom","seat","seat","seat","leave","leave","seat","seat","seat","seat","seat","seat","seat","seat","seat","leave","leave","seat","seat","leave","seat","leave","seat","leave","seat","leave","seat","leave","leave","seat","seat","leave","leave","seat","seat","leave"]
   * [[10],[],[],[],[0],[4],[],[],[],[],[],[],[],[],[],[0],[4],[],[],[7],[],[3],[],[3],[],[9],[],[0],[8],[],[],[0],[8],[],[],[2]]
   *
   * ["ExamRoom","seat","seat","seat","leave","leave","seat","seat","seat","seat","seat","seat","seat"]
   * [[8],[],[],[],[0],[7],[],[],[],[],[],[],[]]
   */

}
