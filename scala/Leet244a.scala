import scala.collection.mutable

object Leet244a extends App {

  class WordDistance(_wordsDict: Array[String]) {

    val storage = _wordsDict.zipWithIndex.groupBy(_._1).map {
      case (word, arr) => (word, arr.map(_._2))
    }

    def shortest(word1: String, word2: String): Int = {
      val queue1 = mutable.Queue.from(storage(word1))
      val queue2 = mutable.Queue.from(storage(word2))
      var minDistance = Int.MaxValue
      var pos1opt: Option[Int] = None
      var pos2opt: Option[Int] = None
      do {
        (pos1opt, pos2opt) match {
          // fill with initial values
          case (None, None) =>
            pos1opt = Some(queue1.dequeue())
            pos2opt = Some(queue2.dequeue())
          // check min distance and switch to next candidate
          case (Some(pos1), Some(pos2)) =>
            minDistance = (pos1 - pos2).abs min minDistance
            if (pos2 > pos1 && queue1.nonEmpty)
              pos1opt = Some(queue1.dequeue())
            else if (queue2.nonEmpty)
              pos2opt = Some(queue2.dequeue())
            else {
              pos1opt = None
              pos2opt = None
            }
          case _ =>
        }
      } while (pos1opt.isDefined && pos2opt.isDefined)
      minDistance
    }

  }
}