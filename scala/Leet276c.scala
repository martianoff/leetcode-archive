object Leet276c extends App {

  object Solution {
    def numWays(n: Int, k: Int): Int = {
      (0 until n).foldLeft(List.empty[Int]) {
          case (aggr, 0) => k :: aggr
          case (aggr, 1) => (k * k) :: aggr
          // on each new turn we have total number of combination equal to
          // (k-1) * last + (k-1) * prev
          // to save memory we limit the length of linked list to 2
          case (last :: prev :: Nil, n) => ((k - 1) * (last + prev)) :: last :: Nil
          case (aggr, _) => aggr
        }
        .head
    }
  }

}
