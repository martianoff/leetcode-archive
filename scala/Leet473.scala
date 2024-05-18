object Leet473 extends App {

  import scala.collection.immutable

  object Solution {
    def makesquare(matchsticks: Array[Int]): Boolean = {
      matchsticks.sum match {
        case totalLength if totalLength % 4 == 0 =>
          val sideLength = totalLength / 4
          val sticksBySize = immutable.TreeMap.from(matchsticks.groupBy(l => l).map {
            case (length, arr) => (length, arr.length)
          })
          println(sticksBySize)
          canBuildSideWithRemainingSticks(
            remainingSides = 4,
            totalSideLength = sideLength,
            remainingSideLength = sideLength,
            remainingSticksBySize = sticksBySize,
            searchFromLength = sideLength)._1
        case _ => false
      }
    }

    def canBuildSideWithRemainingSticks(remainingSides: Int, totalSideLength: Int, remainingSideLength: Int, remainingSticksBySize: immutable.TreeMap[Int, Int], searchFromLength: Int):
    (Boolean, immutable.TreeMap[Int, Int]) = {
      if (remainingSides == 0) {
        return (true, remainingSticksBySize)
      }
      if (remainingSideLength == 0) {
        return canBuildSideWithRemainingSticks(
          remainingSides = remainingSides - 1,
          totalSideLength = totalSideLength,
          remainingSideLength = totalSideLength,
          remainingSticksBySize = remainingSticksBySize,
          searchFromLength = totalSideLength)
      }
      remainingSticksBySize.rangeFrom(searchFromLength).headOption match {
        case Some((length, count)) if count == 1 =>
          canBuildSideWithRemainingSticks(
            remainingSides = remainingSides,
            totalSideLength = totalSideLength,
            remainingSideLength = remainingSideLength - length,
            remainingSticksBySize = remainingSticksBySize - length,
            searchFromLength = remainingSideLength - length) match {
            case (false, _) => canBuildSideWithRemainingSticks(
              remainingSides = remainingSides,
              totalSideLength = totalSideLength,
              remainingSideLength = remainingSideLength,
              remainingSticksBySize = remainingSticksBySize,
              searchFromLength = length - 1)
            case (true, r) => (true, r)
          }
        case Some((length, count)) if count > 0 =>
          canBuildSideWithRemainingSticks(
            remainingSides = remainingSides,
            remainingSideLength = remainingSideLength - length,
            totalSideLength = totalSideLength,
            remainingSticksBySize = remainingSticksBySize + (length -> (count - 1)),
            searchFromLength = remainingSideLength - length) match {
            case (false, _) => canBuildSideWithRemainingSticks(
              remainingSides = remainingSides,
              totalSideLength = totalSideLength,
              remainingSideLength = remainingSideLength,
              remainingSticksBySize = remainingSticksBySize,
              searchFromLength = length - 1)
            case (true, r) => (true, r)
          }
        case _ => (false, remainingSticksBySize)
      }
    }
  }

  object ReverseOrdering extends Ordering[Int] {
    def compare(a: Int, b: Int): Int = b compare a
  }

  println(Solution.makesquare(Array(1, 1, 2, 2, 2)))
  println(Solution.makesquare(Array(3, 3, 3, 3, 4)))
  println(Solution.makesquare(Array(5, 5, 5, 5, 4, 4, 4, 4, 3, 3, 3, 3)))
  println(Solution.makesquare(Array(13, 11, 1, 8, 6, 7, 8, 8, 6, 7, 8, 9, 8)))
  println(Solution.makesquare(Array(4, 13, 1, 1, 14, 15, 1, 3, 13, 1, 3, 5, 2, 8, 12)))

}
