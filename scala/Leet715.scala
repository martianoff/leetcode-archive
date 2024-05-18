import Leet715.Solution.RangeModule

object Leet715 extends App {

  val unitTest = new RangeModule()

  object Solution {

    import scala.util.chaining._
    import scala.collection.mutable

    class RangeModule() {

      private val storage = mutable.TreeMap.empty[Int, Interval]

      def addRange(left: Int, right: Int): Unit = {
        val interval = Interval(left, right - 1)

        if (fullyOverlap(interval).isEmpty) {
          // intervals which ends or start within range
          // +1 is required because [1:8) + [8:9) = [1:9) and [8,9) + [9,10) = [8,10)
          val intersectingIntervals = storage.range(left - 1, right + 1).toList

          intersectingIntervals.foldLeft(interval) {
            case (mergedInterval, (_, anotherInterval)) => mergeIntervals(mergedInterval, anotherInterval)
          }

          if (intersectingIntervals.isEmpty) {
            // if there is no overlapping add interval itself
            storeInterval(interval)
          }
        }
        //println(s"add ${left},${right}, got ${storage}")
      }

      def mergeIntervals(interval1: Interval, interval2: Interval): Interval = {
        deleteInterval(interval1)
        deleteInterval(interval2)
        Interval(interval1.start min interval2.start, interval1.end max interval2.end).tap(storeInterval)
      }

      def queryRange(left: Int, right: Int): Boolean = {
        fullyOverlap(Interval(left, right - 1)).nonEmpty
      }

      private def fullyOverlap(interval: Interval): Option[Interval] = {
        // since intervals don't overlap
        // the last point will show us if this interval overlaps
        storage.rangeTo(interval.start).lastOption match {
          case Some((_, prevInterval)) if prevInterval.start <= interval.start && prevInterval.end >= interval.end
          => Some(prevInterval)
          case _ => None
        }
      }

      def removeRange(left: Int, right: Int): Unit = {
        val removalInterval = Interval(left, right - 1)
        // split intervals
        fullyOverlap(removalInterval) match {
          case Some(fullyOverlapInterval) =>
            deleteInterval(fullyOverlapInterval)
            List(Interval(fullyOverlapInterval.start, removalInterval.start - 1), Interval(removalInterval.end + 1, fullyOverlapInterval.end))
              .filter(_.nonEmpty)
              .foreach(storeInterval)
          case _ =>
            storage.range(left, right).toList.foreach {
              case (point, interval) if point == interval.end =>
                deleteInterval(interval)
                if (interval.start < removalInterval.start) {
                  storeInterval(Interval(interval.start, removalInterval.start - 1))
                }
              case (point, interval) if point == interval.start =>
                deleteInterval(interval)
                if (interval.end > removalInterval.end) {
                  storeInterval(Interval(removalInterval.end + 1, interval.end))
                }
              case _ =>
            }
        }
        //println(s"remove ${left},${right}, got ${storage}")
      }

      private def storeInterval(interval: Interval) = {
        if (interval.nonEmpty) {
          storage += (interval.start -> interval)
          storage += (interval.end -> interval)
        }
      }

      private def deleteInterval(interval: Interval) = {
        if (interval.start == interval.end) {
          storage -= interval.start
        } else {
          storage -= interval.start
          storage -= interval.end
        }
      }

      case class Interval(start: Int, end: Int) {
        def nonEmpty: Boolean = {
          start <= end
        }
      }

    }
  }

  /**
   * println(unitTest.addRange(10,20))
   * println(unitTest.removeRange(14,16))
   * println(unitTest.queryRange(10,14))
   * println(unitTest.queryRange(13,15))
   * println(unitTest.queryRange(16,17))
   * */

  /**
   * ["RangeModule","addRange","removeRange","removeRange","addRange","removeRange","addRange","queryRange","queryRange","queryRange"]
   * [[],[6,8],[7,8],[8,9],[8,9],[1,3],[1,8],[2,4],[2,9],[4,6]]
   *
   *
   * println(unitTest.addRange(6,8))
   * println(unitTest.removeRange(7,8))
   * println(unitTest.removeRange(8,9))
   * println(unitTest.addRange(8,9))
   * println(unitTest.removeRange(1,3))
   * println(unitTest.addRange(1,8))
   * println(unitTest.queryRange(2,4))
   * println(unitTest.queryRange(2,9))
   * println(unitTest.queryRange(4,6))
   */
  /**
   * ["RangeModule","addRange","queryRange","addRange","queryRange","addRange","removeRange","removeRange","removeRange","queryRange"]
   * [[],[5,7],[2,7],[6,9],[2,9],[2,7],[3,10],[1,8],[1,10],[4,7]]
   *
   *
   * println(unitTest.addRange(5, 7))
   * println(unitTest.queryRange(2, 7))
   * println(unitTest.addRange(6, 9))
   * println(unitTest.queryRange(2, 9))
   * println(unitTest.addRange(2, 7))
   * println(unitTest.removeRange(3, 10))
   * println(unitTest.removeRange(1, 8))
   * println(unitTest.removeRange(1, 10))
   * println(unitTest.queryRange(4, 7))
   */

  /**
   * ["RangeModule","addRange","addRange","addRange","queryRange","queryRange","queryRange","removeRange","queryRange"]
   * [[],[10,180],[150,200],[250,500],[50,100],[180,300],[600,1000],[50,150],[50,100]]
   *
   * println(unitTest.addRange(10, 180))
   * println(unitTest.addRange(150, 200))
   * println(unitTest.addRange(250, 500))
   * println(unitTest.queryRange(50, 100))
   * println(unitTest.queryRange(180, 300)) //false
   * println(unitTest.queryRange(600, 1000))
   * println(unitTest.removeRange(50, 150))
   * println(unitTest.queryRange(50, 100))
   */

  /**
   * ["RangeModule","addRange","addRange","queryRange","queryRange","queryRange","removeRange","addRange","removeRange","addRange","removeRange","removeRange","queryRange","queryRange","queryRange","queryRange","removeRange","addRange","removeRange","queryRange","addRange","addRange","removeRange","queryRange","removeRange","removeRange","removeRange","addRange","removeRange","addRange","queryRange","queryRange","queryRange","queryRange","queryRange","addRange","removeRange","addRange","addRange","addRange","queryRange","removeRange","addRange","queryRange","addRange","queryRange","removeRange","removeRange","addRange","addRange","queryRange","queryRange","addRange","addRange","removeRange","removeRange","removeRange","queryRange","removeRange","removeRange","addRange","queryRange","removeRange","addRange","addRange","queryRange","removeRange","queryRange","addRange","addRange","addRange","addRange","addRange","removeRange","removeRange","addRange","queryRange","queryRange","removeRange","removeRange","removeRange","addRange","queryRange","removeRange","queryRange","addRange","removeRange","removeRange","queryRange"]
   * [[],[44,53],[69,89],[23,26],[80,84],[11,12],[86,91],[87,94],[34,52],[1,59],[62,96],[34,83],[11,59],[59,79],[1,13],[21,23],[9,61],[17,21],[4,8],[19,25],[71,98],[23,94],[58,95],[12,78],[46,47],[50,70],[84,91],[51,63],[26,64],[38,87],[41,84],[19,21],[18,56],[23,39],[29,95],[79,100],[76,82],[37,55],[30,97],[1,36],[18,96],[45,86],[74,92],[27,78],[31,35],[87,91],[37,84],[26,57],[65,87],[76,91],[37,77],[18,66],[22,97],[2,91],[82,98],[41,46],[6,78],[44,80],[90,94],[37,88],[75,90],[23,37],[18,80],[92,93],[3,80],[68,86],[68,92],[52,91],[43,53],[36,37],[60,74],[4,9],[44,80],[85,93],[56,83],[9,26],[59,64],[16,66],[29,36],[51,96],[56,80],[13,87],[42,72],[6,56],[24,53],[43,71],[36,83],[15,45],[10,48]]
   *
   *
   * println(unitTest.addRange(44, 53))
   * println(unitTest.addRange(68, 89))
   * println(unitTest.removeRange(86, 91))
   * println(unitTest.addRange(87,94))
   * println(unitTest.removeRange(34,52))
   * println(unitTest.addRange(1,59))
   * println(unitTest.removeRange(62,96))
   * println(unitTest.removeRange(34,83))
   * println(unitTest.queryRange(11,59)) //false
   */

  /**
   * ["RangeModule","addRange","removeRange","addRange","queryRange","removeRange","addRange","queryRange","addRange","addRange","addRange",]
   * [[],[17,63],[78,90],[9,18],[51,74],[20,54],[35,72],[2,29],[28,41],[17,95],[73,75],
 */

  println(unitTest.addRange(17, 63))
  println(unitTest.removeRange(78, 90))
  println(unitTest.addRange(9, 18))
  println(unitTest.queryRange(51, 74))
  println(unitTest.removeRange(20, 54))
  println(unitTest.addRange(35, 72))
  println(unitTest.queryRange(2, 29))
  println(unitTest.addRange(28, 41))
  println(unitTest.addRange(17, 95))
  println(unitTest.addRange(73, 75))

  /*
  *  "queryRange","addRange","queryRange","removeRange","removeRange","removeRange","removeRange","queryRange","removeRange","queryRange",
     *  [34,43],[57,96],[51,72],[21,67],[40,73],[14,26],[71,86],[34,41],[10,25],[27,68],
   */
  println(unitTest.queryRange(34, 43))
  println(unitTest.addRange(57, 96))
  println(unitTest.queryRange(51, 72))
  println(unitTest.removeRange(21, 67))
  println(unitTest.removeRange(40, 73))
  println(unitTest.removeRange(14, 26))
  println(unitTest.removeRange(71, 86))
  println(unitTest.queryRange(34, 41))
  println(unitTest.removeRange(10, 25))
  println(unitTest.queryRange(27, 68))

  /*
  *  "queryRange","removeRange","queryRange","addRange","addRange","queryRange","removeRange","removeRange","queryRange","addRange"
  *  [18,32],[30,31],[45,61],[64,66],[18,93],[13,21],[13,46],[56,99],[6,93],[25,36]
  * */
  println(unitTest.queryRange(18, 32))
  println(unitTest.removeRange(30, 31))
  println(unitTest.queryRange(45, 61))
  println(unitTest.addRange(64, 66))
  println(unitTest.addRange(18, 93))
  println(unitTest.queryRange(13, 21))
  println(unitTest.removeRange(13, 46))
  println(unitTest.removeRange(56, 99))
  println(unitTest.queryRange(6, 93))
  println(unitTest.addRange(25, 36))

  /**
   * ,"removeRange","removeRange","addRange","addRange","addRange","queryRange","queryRange","addRange","queryRange","removeRange","queryRange","removeRange","addRange","queryRange"
   * [27,88],[82,83],[30,71],[31,73],[10,41],[71,72],[9,56],[22,76],[38,74],[2,77],[33,61],[74,75],[11,43],[27,75]]
   */

  println(unitTest.removeRange(27, 88))
  println(unitTest.removeRange(82, 83))
  println(unitTest.addRange(30, 71))
  println(unitTest.addRange(31, 73))
  println(unitTest.addRange(10, 41))
  println(unitTest.queryRange(71, 72))
  println(unitTest.queryRange(9, 56)) //true
}
