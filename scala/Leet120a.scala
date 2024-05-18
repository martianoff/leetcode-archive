object Leet120a extends App {
  object Solution {
    def minimumTotal(triangle: List[List[Int]]): Int = {
      triangle.foldLeft(Array.empty[Int]) {
        case (bestSoFar, row) => {
          row.zipWithIndex.toArray.map {
            // init the first row
            case (v, _) if bestSoFar.isEmpty => v
            // left most number
            case (v, i) if i == 0 => (v + bestSoFar(i))
            // right most number
            case (v, i) if i == bestSoFar.length => (v + bestSoFar(i - 1))
            // general case
            case (v, i) => (v + bestSoFar(i - 1)) min (v + bestSoFar(i))
          }
        }
      }.min
    }
  }
}
