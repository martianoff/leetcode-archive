import scala.collection.mutable

object Leet1058 extends App {

  class Number(numStr: String) {
    lazy val floor = numInt - numInt % 1000
    lazy val ceil = if (numInt != floor) {
      floor + 1000
    } else {
      floor
    }
    private val numInt = numStr.split('.') match {
      case Array(a, b) => a.toInt * 1000 + b.toInt
      case _ => throw new Exception("Invalid input")
    }

    def toInt() = {
      numInt
    }
  }

  class Dfs(prices: Array[Number], target: Int) {
    private val target1000 = target * 1000
    private val cache = mutable.Set.empty[(Int, Int, Int)]
    private var result: Option[Int] = None

    def getResult(): Option[Int] = {
      result
    }

    def search(priceId: Int, sum: Int, diff: Int): Unit = {
      if (result.exists(_ < diff) || sum > target1000 || cache.contains((priceId, sum, diff))) {
        return
      } else {
        cache += ((priceId, sum, diff))
        if (priceId == prices.length) {
          if (sum == target1000) {
            result = Some(diff)
          }
        } else {
          search(priceId + 1, sum + prices(priceId).floor, diff + (prices(priceId).toInt - prices(priceId).floor).abs)
          if (prices(priceId).floor != prices(priceId).ceil) {
            search(priceId + 1, sum + prices(priceId).ceil, diff + (prices(priceId).toInt - prices(priceId).ceil).abs)
          }
        }
      }

    }

    search(0, 0, 0)
  }

  object Solution {
    def minimizeError(prices: Array[String], target: Int): String = {
      new Dfs(prices.map(new Number(_)), target)
        .getResult
        .map(r => "%.3f".format(r.toDouble / 1000))
        .getOrElse("-1")
    }

  }
}
