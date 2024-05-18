object Leet284 extends App {

  import scala.util.chaining._

  class PeekingIterator(_iterator: Iterator[Int]) {
    var nextValue: Option[Int] = refreshNext()

    def peek(): Int = {
      nextValue.get
    }

    def next(): Int = {
      nextValue.get.tap {
        _ => nextValue = refreshNext()
      }
    }

    private def refreshNext(): Option[Int] = {
      _iterator.hasNext match {
        case true => Some(_iterator.next)
        case _ => None
      }
    }

    def hasNext(): Boolean = {
      nextValue.nonEmpty
    }
  }

}
