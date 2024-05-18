import scala.collection.mutable

object Leet1055 extends App {

  object Solution {
    def shortestWay(source: String, target: String): Int = {
      if (!target.toSet.subsetOf(source.toSet)) return -1
      Iterator.unfold((source.toList, target.toList)) {
        // target constructed
        case (_, Nil) => None
        // source sequence is used, try another sequence
        case (Nil, target) => Some(1, (source.toList, target))
        // if matches eliminate as much as possible
        case (sourceHead :: sourceTail, targetHead :: targetTail) if sourceHead == targetHead => Some(0, (sourceTail, targetTail))
        // if doesn't match try next combination
        case (_ :: sourceTail, target) => Some(0, (sourceTail, target))
      }.sum + 1 //add one because final combination always give 1
    }
  }
}
