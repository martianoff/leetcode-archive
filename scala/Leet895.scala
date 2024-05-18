object Leet895 extends App {

  import scala.collection.mutable
  import scala.util.chaining._

  import scala.collection.mutable
  import scala.util.chaining._

  class FreqStack() {

    // sorted tree of frequency to a sorted set of indices
    private val frequencyToIndex = mutable.TreeMap.empty[Int, mutable.TreeSet[Int]]
    // indexes to value mapping
    private val indexToValueMap = mutable.Map.empty[Int, Int]
    // value to frequency mapping
    private val valueToFrequencyMap = mutable.Map.empty[Int, Int]

    // add value to maintaining 3 data structures
    def push(`val`: Int) {
      val index = indexToValueMap.size
      indexToValueMap(indexToValueMap.size) = `val`
      val newFrequency = valueToFrequencyMap.get(`val`).getOrElse(0) + 1
      valueToFrequencyMap(`val`) = newFrequency
      // collect old existing indices from old frequency
      val oldIndices: List[Int] = if (frequencyToIndex.contains(newFrequency - 1)) {
        frequencyToIndex(newFrequency - 1).toList.collect {
          case index if indexToValueMap(index) == `val` => index
        }.tap {
          // remove old frequency
          list =>
            frequencyToIndex(newFrequency - 1) --= list
            if (frequencyToIndex(newFrequency - 1).isEmpty) {
              frequencyToIndex -= newFrequency - 1
            }
        }
      } else List()
      // add new frequency and move old indices
      if (!frequencyToIndex.contains(newFrequency)) {
        frequencyToIndex += (newFrequency -> mutable.TreeSet.empty[Int])
      }
      frequencyToIndex(newFrequency) ++= index :: oldIndices
    }

    // pop value maintaining 3 data structures
    def pop(): Int = {
      frequencyToIndex.last match {
        case (maxFrequency, indices) =>
          val lastIndex = indices.last
          indexToValueMap(lastIndex).tap {
            value =>
              indexToValueMap - lastIndex
              val newFrequency = valueToFrequencyMap.get(value).getOrElse(0) - 1
              if (newFrequency == 0) {
                valueToFrequencyMap -= value
              } else {
                valueToFrequencyMap(value) = newFrequency
              }
              // collect old existing indices from old frequency
              val oldIndices: List[Int] = if (frequencyToIndex.contains(newFrequency + 1)) {
                frequencyToIndex(newFrequency + 1).toList.collect {
                  case index if index != lastIndex && indexToValueMap(index) == value => index
                }.tap {
                  // remove old frequency
                  list =>
                    frequencyToIndex(newFrequency + 1) --= lastIndex :: list
                    if (frequencyToIndex(newFrequency + 1).isEmpty) {
                      frequencyToIndex -= newFrequency + 1
                    }
                }
              } else List()
              // add new frequency and move old indices without removed one
              if (!frequencyToIndex.contains(newFrequency)) {
                frequencyToIndex += (newFrequency -> mutable.TreeSet.empty[Int])
              }
              frequencyToIndex(newFrequency) ++= oldIndices
          }
      }
    }

  }

}
