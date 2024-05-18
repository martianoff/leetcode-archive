object Leet731 extends App {

  import scala.collection.mutable
  import scala.util.chaining._

  val c3 = new MyCalendarTwo

  case class Interval(start: Int, end: Int)

  /*
  val c1 = new MyCalendarTwo
  List(
  (10,20),
  (50,60),
  (10,40),
  (5,15),
  (5,10),
  (25,55)
  ).foreach {
    case (a,b) => println(s"for [${a},${b}] : " + c1.book(a,b))
  }
   */

  /*
  val c2 = new MyCalendarTwo
  List(
    (47,50),
    (1,10),
    (27,36),
    (40,47),
    (20,27),
    (15,23),
    (10,18),
    (27,36),
    (17,25),
    (8,17),
    (24,33),
    (23,28), // should be false
    (21,27)
  ).foreach {
    case (a, b) => println(s"for [${a},${b}] : " + c2.book(a, b))
  }
  */

  class MyCalendarTwo() {

    val calendar1: mutable.TreeMap[Int, Set[Interval]] = mutable.TreeMap.empty[Int, Set[Interval]]
    val calendar2: mutable.TreeMap[Int, Set[Interval]] = mutable.TreeMap.empty[Int, Set[Interval]]

    def book(start: Int, end: Int): Boolean = {
      val newInterval = Interval(start, end - 1)
      // if there is intersection with calendar2 return false
      if (intersections(interval = newInterval, calendar = calendar2).nonEmpty) {
        return false
      }
      // find intersection with calendar1 and move them to calendar2
      intersections(interval = newInterval, calendar = calendar1)
        .tap {
          _ => addInterval(newInterval, calendar1)
        }
        .foreach(interval => addInterval(interval, calendar2))
      true
    }

    def addInterval(interval: Interval, calendar: mutable.TreeMap[Int, Set[Interval]]): Unit = {
      calendar(interval.start) = calendar.get(interval.start).map(_ + interval).getOrElse(Set(interval))
      calendar(interval.end) = calendar.get(interval.end).map(_ + interval).getOrElse(Set(interval))
    }

    def intersections(interval: Interval, calendar: mutable.TreeMap[Int, Set[Interval]]): List[Interval] = {
      // new range contains existing ranges inside
      val insideIntersections = calendar.range(interval.start, interval.end + 1).flatMap {
        case (_, anotherIntervals) =>
          anotherIntervals.map(intersection(interval, _))
      }.toList
      // new range is within existing range
      val outsideIntersection = calendar.until(interval.start).find {
        case (_, i) if i.exists(i => i.end >= interval.end) => true
        case (_, _) => false
      }.map(_ => interval)
      (outsideIntersection :: insideIntersections).collect {
        case Some(i) => i
      }
    }

    def intersection(interval1: Interval, interval2: Interval): Option[Interval] = {
      (interval1, interval2) match {
        case (Interval(start1, end1), Interval(start2, end2)) if (start1 <= start2 && end1 >= start2) || (start2 <= start1 && end2 >= start1) =>
          Some(Interval(start1 max start2, end1 min end2))
        case _ => None
      }
    }

  }

  List(
    (44, 50),
    (47, 50),
    (9, 15),
    (4, 10),
    (2, 7),
    (28, 37), // should be true
    (26, 33), // should be true
    (22, 28) // should be true
  ).foreach {
    case (a, b) => println(s"for [${a},${b}] : " + c3.book(a, b))
  }
}
