object Leet855 extends App {

  import scala.collection.mutable
  import scala.util.chaining._

  val test = new ExamRoom(10)

  class ExamRoom(_n: Int) {

    val beginOfIntervalToInterval = mutable.TreeMap.empty[Int, Int]
    val intervalsToBeginOfInterval = mutable.TreeMap.empty[Int, mutable.TreeSet[Int]]

    def seat(): Int = {
      (intervalsToBeginOfInterval.lastOption match {
        // nothing on the left side
        case Some((_, intervalStarts)) if !beginOfIntervalToInterval.contains(1) =>
          (1, beginOfIntervalToInterval.headOption.map(_._1).getOrElse(_n) - 2)
        // nothing on the right side
        case Some((intervalLength, intervalStarts)) if !beginOfIntervalToInterval.contains(_n) =>
          (_n, 0)
        // regular case - pick the longest interval
        case Some((intervalLength, intervalStarts)) =>
          // there is also another possible option with longest interval - 1 and lower start index
          // we need to pick an option with the lowest start index
          val alternativeIntervalLength = intervalLength - 1
          intervalsToBeginOfInterval.get(alternativeIntervalLength) match {
            case Some(alternativeIntervalStarts) if alternativeIntervalStarts.head < intervalStarts.head && alternativeIntervalLength % 2 != 0 && alternativeIntervalLength > 0 =>
              (alternativeIntervalStarts.head + ((alternativeIntervalLength + 1) / 2), (alternativeIntervalLength - 1) / 2)
            case Some(alternativeIntervalStarts) if alternativeIntervalStarts.head < intervalStarts.head && alternativeIntervalLength > 0 =>
              (alternativeIntervalStarts.head + (alternativeIntervalLength / 2), alternativeIntervalLength / 2)
            case _ if intervalLength % 2 != 0 =>
              (intervalStarts.head + (intervalLength + 1) / 2, (intervalLength - 1) / 2)
            case _ =>
              (intervalStarts.head + intervalLength / 2, intervalLength / 2)
          }
        // empty room
        case _ =>
          (1, _n - 2)
      }) match {
        // save to the storage
        case (start, length) =>
          // add new interval to the storage
          println(s"selected interval at ${start} with len ${length}")
          storeInterval(start, length)
          (start - 1)
            .tap {
              pos =>
                println(s"seat at ${pos}")
                println(beginOfIntervalToInterval)
                println(intervalsToBeginOfInterval)
            }
      }
    }

    private def storeInterval(start: Int, length: Int) = {
      beginOfIntervalToInterval += (start -> length)
      if (!intervalsToBeginOfInterval.contains(length)) {
        intervalsToBeginOfInterval(length) = mutable.TreeSet.empty[Int]
      }
      intervalsToBeginOfInterval(length) += start
      // adjust prev interval if exists
      beginOfIntervalToInterval.rangeUntil(start).lastOption match {
        case Some((prevIntervalPos, prevIntervalLength)) =>
          val prevIntervalNewLength = start - prevIntervalPos - 1
          beginOfIntervalToInterval(prevIntervalPos) = prevIntervalNewLength
          intervalsToBeginOfInterval(prevIntervalLength) -= prevIntervalPos
          if (intervalsToBeginOfInterval(prevIntervalLength).isEmpty) {
            intervalsToBeginOfInterval -= prevIntervalLength
          }
          if (!intervalsToBeginOfInterval.contains(prevIntervalNewLength)) {
            intervalsToBeginOfInterval(prevIntervalNewLength) = mutable.TreeSet.empty[Int]
          }
          intervalsToBeginOfInterval(prevIntervalNewLength) += prevIntervalPos
        case _ =>
      }
    }

    def leave(p: Int): Unit = {
      removeInterval(p + 1)
      println(s"removed seat at ${p}")
      println(beginOfIntervalToInterval)
      println(intervalsToBeginOfInterval)
    }

    private def removeInterval(start: Int): Unit = {
      if (!beginOfIntervalToInterval.contains(start)) {
        return
      }
      val intervalLength = beginOfIntervalToInterval(start)
      beginOfIntervalToInterval.rangeUntil(start - 1).lastOption match {
        // increase prev interval if exists
        case Some((prevIntervalPos, prevIntervalLength)) if prevIntervalLength > 0 =>
          val prevIntervalNewLength = prevIntervalLength + intervalLength + 1
          beginOfIntervalToInterval(prevIntervalPos) = prevIntervalNewLength
          intervalsToBeginOfInterval(prevIntervalLength) -= -prevIntervalPos
          if (intervalsToBeginOfInterval(prevIntervalLength).isEmpty) {
            intervalsToBeginOfInterval -= prevIntervalLength
          }
          if (!intervalsToBeginOfInterval.contains(prevIntervalNewLength)) {
            intervalsToBeginOfInterval(prevIntervalNewLength) = mutable.TreeSet.empty[Int]
          }
          intervalsToBeginOfInterval(prevIntervalNewLength) += prevIntervalPos
          beginOfIntervalToInterval -= start
        // if it is the first real interval remove it
        case None if intervalLength > 0 =>
          beginOfIntervalToInterval -= start
        // if prev interval has 0 length we should make a new interval instead
        case _ =>
          // create new interval
          if (!intervalsToBeginOfInterval.contains(1)) {
            intervalsToBeginOfInterval(1) = mutable.TreeSet.empty[Int]
          }
          intervalsToBeginOfInterval(1) += (start - 1)
          beginOfIntervalToInterval(start - 1) = 1
      }
      intervalsToBeginOfInterval(intervalLength) -= start
      if (intervalsToBeginOfInterval(intervalLength).isEmpty) {
        intervalsToBeginOfInterval -= intervalLength
      }
    }

  }

  println(test.seat())
  println(test.seat())
  println(test.seat())
  println(test.seat())
  println(test.leave(4))
  println(test.seat())

  /**
   * ["ExamRoom","seat","seat","seat","seat","leave","seat"]
   * [[10],[],[],[],[],[4],[]]
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
   *
   */

}
