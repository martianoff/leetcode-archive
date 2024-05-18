object Leet118 extends App {
  object Solution {
    def generate(numRows: Int): List[List[Int]] = {
      Iterator.unfold(List(1)) {
        prevRow =>
          Some((prevRow, 1 :: prevRow.foldLeft(List(1), List.empty[Int]) {
            case ((aggr, a :: Nil), v) =>
              ((a + v) :: aggr, v :: Nil)
            case ((aggr, buf), v) =>
              (aggr, v :: buf)
          }._1))
      }.take(numRows).toList
    }
  }

  println(Solution.generate(5))
}
