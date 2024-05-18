import scala.util.chaining.scalaUtilChainingOps

object Leet1981b extends App {

  object Solution {
    def minimizeTheDifference(mat: Array[Array[Int]], target: Int): Int = mat
      .foldLeft(Set(0), Option.empty[Int]) {
        case lessThanTarget -> noLessThanTarget -> row => lessThanTarget
          .iterator
          .concat(noLessThanTarget)
          .flatMap(sum => row.iterator.map(sum + _))
          .partition(_ < target)
          .pipe { case lessThanTarget -> noLessThanTarget => lessThanTarget.toSet -> noLessThanTarget.minOption }
      }
      .pipe {
        case lessThanTarget -> noLessThanTarget => lessThanTarget
          .iterator
          .concat(noLessThanTarget)
          .map(_ - target)
          .map(math.abs)
          .min
      }
  }

}
