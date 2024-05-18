import scala.collection.mutable

object Leet502 {

  def findMaximizedCapital(k: Int, w: Int, profits: Array[Int], capital: Array[Int]): Int = {
    var currentCapital = w
    val accessibleDeals = mutable.TreeMap.from(
      capital.zipWithIndex.map {
        case (c, i) => (c, Deal(c, profits(i)))
      }.groupBy {
        case (c, _) => c
      }.map {
        case (c, arr) => (c, arr.map(_._2))
      }
    )(Asc)
    val bestAvailableDeals = mutable.PriorityQueue.empty(DealDesc)
    (0 until k).foreach { _ =>
      // select affordable range of deals
      val affordableDeals = accessibleDeals
        .rangeTo(currentCapital)
        .toList
      // merge afforable deals into bestAvailableDeals
      affordableDeals.foreach {
        case (capital, deals) =>
          deals.foreach {
            deal => bestAvailableDeals.enqueue(deal)
          }
          accessibleDeals -= capital
      }
      // nothing to pick
      if (bestAvailableDeals.isEmpty) {
        return currentCapital
      }
      // select best available and affordable deal
      // can pick a deal
      bestAvailableDeals.dequeue() match {
        case Deal(capital, profit) =>
          currentCapital += profit
          accessibleDeals -= capital
      }
    }
    currentCapital
  }

  case class Deal(capital: Int, profit: Int)

  object DealDesc extends Ordering[Deal] {
    def compare(a: Deal, b: Deal) = {
      a.profit compare b.profit
    }
  }

  object Asc extends Ordering[Int] {
    def compare(a: Int, b: Int) = {
      a compare b
    }
  }
}