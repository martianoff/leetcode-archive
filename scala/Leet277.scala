object Leet277 extends App {

  /* The knows API is defined in the parent class Relation.
        def knows(a: Int, b: Int): Boolean = {} */

  import scala.collection.mutable

  def knows(i: Int, i1: Int) = {
    true
  }

  trait Relation

  class Solution extends Relation {
    def findCelebrity(n: Int): Int = {
      val indegree = mutable.Map.empty[Int, Int].withDefaultValue(0)
      val outdegree = mutable.Map.empty[Int, Int].withDefaultValue(0)
      // BOTH INNER AND OUTER LOOP CAN BE FURTHER OPTIMIZED
      (0 until n).foreach {
        a =>
          (0 until n).foreach {
            case b if a != b && outdegree(a) == 0 =>
              if (knows(b, a)) {
                indegree(a) += 1
              }
              if (knows(a, b)) {
                outdegree(a) += 1
              }
              if (indegree(a) == n - 1 && outdegree(a) == 0) {
                return a
              }
            case _ =>
          }
      }
      -1
    }
  }

}
