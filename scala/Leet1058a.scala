import scala.collection.mutable

object Leet1058a extends App {

  import scala.collection.mutable

  class Number(numStr: String) {
    val floor = numInt - numInt % 1000
    val ceil = if (numInt != floor) {
      floor + 1000
    } else {
      floor
    }
    private val numInt = numStr.split('.') match {
      case Array(a, b) => a.toInt * 1000 + b.toInt
      case _ => throw new Exception("Invalid input")
    }

    def toInt = numInt
  }

  object Solution {
    def minimizeError(prices: Array[String], target: Int): String = {
      val nums = prices.map(new Number(_))
      val minDiff = nums.map(_.floor).sum
      val maxDiff = nums.map(_.ceil).sum
      if (minDiff > target * 1000 || maxDiff < target * 1000) {
        return "-1"
      }
      // do not include numbers where ceil operation doesn't give increment of a number
      val ceilCosts = mutable.PriorityQueue.from(nums.collect {
        case n if n.ceil != n.toInt => n.ceil - n.toInt
      })(Asc)
      var numberOfCeilRequired = (target * 1000 - minDiff) / 1000
      var costPaidToFloor = nums.map(n => n.toInt - n.floor).sum
      while (numberOfCeilRequired > 0) {
        val costToCeil = ceilCosts.dequeue()
        // since costPaidToFloor already includes floor cost for every option,
        // and we want to do ceil instead of floor,
        // we need to deduct the paid cost first
        costPaidToFloor -= (1000 - costToCeil)
        // add cost to ceil
        costPaidToFloor += costToCeil
        numberOfCeilRequired -= 1
      }
      "%.3f".format(costPaidToFloor.toDouble / 1000)
    }

    object Asc extends Ordering[Int] {
      def compare(a: Int, b: Int) = {
        b compare a
      }
    }

  }
}
