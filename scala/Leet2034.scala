import scala.collection.mutable

object Leet2034 extends App {
  class StockPrice() {

    val priceStorage: mutable.Map[Int, mutable.Set[Int]] = mutable.TreeMap.empty[Int, mutable.Set[Int]]
    val lastPricesAtTs = mutable.Map.empty[Int, Int]
    var curTimestamp = 0

    // O(log N)
    def update(timestamp: Int, price: Int): Unit = {
      curTimestamp = curTimestamp.max(timestamp)
      // remove old record
      lastPricesAtTs.get(timestamp) match {
        case Some(oldPrice) => {
          priceStorage(oldPrice) -= timestamp
          if (priceStorage(oldPrice).isEmpty) {
            priceStorage.remove(oldPrice)
          }
        }
        case _ =>
      }
      lastPricesAtTs += (timestamp -> price)
      // add new record
      priceStorage(price) = priceStorage.get(price) match {
        case Some(timestampSet) =>
          timestampSet += timestamp
        case None =>
          mutable.Set(timestamp)
      }
    }

    // O(1)
    def current(): Int = {
      lastPricesAtTs(curTimestamp)
    }

    // O(log N)
    def maximum(): Int = {
      priceStorage.lastOption match {
        case Some((price, _)) => price
        case _ => -1
      }
    }

    // O(log N)
    def minimum(): Int = {
      priceStorage.headOption match {
        case Some((price, _)) => price
        case _ => -1
      }
    }

  }
}
