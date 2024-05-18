object Leet122b extends App {

  object Solution {
    def maxProfit(prices: Array[Int]): Int = {
      prices.foldLeft((0, Option.empty[Int])) {
        // first element
        case ((maxProfit, None), price) => (maxProfit, Some(price))
        // price went up
        case ((maxProfit, Some(prevPrice)), price) if price > prevPrice =>
          (maxProfit + price - prevPrice, Some(price))
        // price went down
        case ((maxProfit, Some(prevPrice)), price) =>
          (maxProfit, Some(price))
      }._1
    }
  }
}
