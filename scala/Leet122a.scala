object Leet122a extends App {

  import scala.collection.mutable
  import scala.util.chaining._

  object Solution {

    def maxProfit(prices: Array[Int]): Int = {
      val cache = mutable.Map.empty[(Int, Int, Option[Int]), Int]
      search(cache, prices)
    }

    private def cached(
                        cache: mutable.Map[(Int, Int, Option[Int]), Int]
                      )(
                        key: (Int, Int, Option[Int])
                      )(f: => Int): Int = {
      cache.get(key) match {
        case Some(r) =>
          r
        case None =>
          f.tap {
            r => cache += (key -> r)
          }
      }
    }

    private def search(cache: mutable.Map[(Int, Int, Option[Int]), Int], prices: Array[Int], day: Int = 0, profit: Int = 0, ownStock: Option[Int] = None): Int = {
      if (day == prices.length) {
        return profit
      }
      cached(cache)(day, profit, ownStock) {
        ownStock match {
          // potential profit
          case Some(value) if prices(day) > value =>
            // can sell
            search(cache, prices, day + 1, profit + (prices(day) - value), None) max
              // or sell and then buy or do nothing
              search(cache, prices, day, profit + (prices(day) - value), None) max
              // or do nothing
              search(cache, prices, day + 1, profit, Some(value))
          // no profit
          case Some(value) =>
            // do nothing
            search(cache, prices, day + 1, profit, Some(value))
          // nothing to sell
          case None =>
            // can do nothing
            search(cache, prices, day + 1, profit, None) max
              // or buy
              search(cache, prices, day + 1, profit, Some(prices(day)))
        }
      }
    }
  }
}
