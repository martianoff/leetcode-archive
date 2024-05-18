object Leet1570 extends App {

  class SparseVector(nums: Array[Int]) {
    // convert nums to Map(index -> value) and remove all 0 because they won't contribute to the sum
    val items: Map[Int, Int] = nums.zipWithIndex.filter(_._1 > 0).map { case (v, i) => (i, v) }.toMap

    def apply(key: Int): Option[Int] = {
      items.get(key)
    }

    def dotProduct(vec: SparseVector): Int = {
      items.foldLeft(0) {
        case (sum, (i, v1)) => vec(i) match {
          case Some(v2) => sum + v1 * v2
          case _ => sum
        }
        case (sum, _) => sum
      }
    }
  }

}
