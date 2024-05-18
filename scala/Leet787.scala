object Leet787 extends App {
  object Solution {
    def findCheapestPrice(n: Int, flights: Array[Array[Int]], src: Int, dst: Int, k: Int): Int = {
      // Bellman-Ford
      val bestPricesToDst = Array.fill(n)(Option.empty[Int])
      bestPricesToDst(src) = Some(0)
      (0 to k).foreach {
        _ =>
          val tempPrices = bestPricesToDst.clone()
          flights.foreach {
            case Array(from, to, price) =>
              bestPricesToDst(from) match {
                case Some(priceToSrc) if priceToSrc + price < tempPrices(to).getOrElse(Int.MaxValue) =>
                  tempPrices(to) = Some(priceToSrc + price)
                // skip destinations if there are no known path to them
                // or if they don't improve the result
                case _ =>
              }
          }
          (0 until n).foreach(i => bestPricesToDst(i) = tempPrices(i))
      }
      bestPricesToDst(dst).getOrElse(-1)
    }
  }
}
