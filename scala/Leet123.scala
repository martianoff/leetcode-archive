object Leet123 extends App {

  import scala.collection.mutable
  import scala.util.chaining._

  object Solution {

    def maxProfit(prices: Array[Int]): Int = {
      val cache = mutable.Map.empty[(Int, Int, Boolean), Int]
      search(cache, prices)
    }

    private def cached(
                        cache: mutable.Map[(Int, Int, Boolean), Int]
                      )(
                        key: (Int, Int, Boolean)
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

    private def search(cache: mutable.Map[(Int, Int, Boolean), Int], prices: Array[Int], day: Int = 0, haveStock: Boolean = false, transactions: Int = 0): Int = {
      // transaction limit
      if (transactions == 2 || day == prices.length) {
        return 0
      }
      cached(cache)(day, transactions, haveStock) {
        if (haveStock) {
          // potential profit
          // can sell
          (prices(day) + search(cache, prices, day + 1, haveStock = false, transactions + 1)) max
            // or do nothing
            search(cache, prices, day + 1, haveStock = true, transactions)
        } else {
          // nothing to sell
          // can do nothing
          search(cache, prices, day + 1, haveStock = false, transactions) max
            // or buy
            (-prices(day) + search(cache, prices, day + 1, haveStock = true, transactions))
        }
      }
    }

  }
}
