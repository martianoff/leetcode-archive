object Leet120b extends App {
  object Solution {
    def minimumTotal(triangle: List[List[Int]]): Int = {
      val buffer = Array.fill(triangle.last.size)(Option.empty[Int])
      triangle.foreach {
        row => {
          row.zipWithIndex.reverse.foreach {
            // left most number
            case (v, i) if i == 0 =>
              buffer(i) = Some(v + buffer(i).getOrElse(0))
            // right most number
            case (v, i) if i == row.length - 1 =>
              buffer(i) = Some(v + buffer(i - 1).getOrElse(0))
            // general case
            case (v, i) =>
              buffer(i) = Some((v + buffer(i - 1).getOrElse(0)) min (v + buffer(i).getOrElse(0)))
          }
        }
      }
      buffer.collect {
        case Some(n) => n
      }.min
    }
  }
}
