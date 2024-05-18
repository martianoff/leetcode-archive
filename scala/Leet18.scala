object Leet18 extends App {
  object Solution {

    def fourSum(nums: Array[Int], target: Int): List[List[Int]] = {
      nums.zipWithIndex.flatMap {
          case (v3, i3) => nums.zipWithIndex.collect {
            case (v4, i4) if i3 != i4 => (v4 + v3, i3, i4)
          }
        }
        .groupBy(_._1)
        .map {
          case (k, tuples) => (k, tuples.map(t => (t._2, t._3)))
        } match {
        case targetMap =>
          nums.zipWithIndex.foldLeft(Set.empty[NumSet]) {
              case (s, (v1, i1)) =>
                s ++ nums.zipWithIndex.foldLeft(Set.empty[NumSet]) {
                  case (sl, (v2, i2)) if i1 != i2 =>
                    (targetMap.get(target - v1 - v2) match {
                      case Some(tuples) =>
                        tuples.collect {
                          case (i3, i4) if i3 != i1 && i3 != i2 && i4 != i1 && i4 != i2 =>
                            new NumSet(v1, v2, nums(i3), nums(i4))
                        } match {
                          case s if s.length > 0 => Some(s)
                          case _ => None
                        }
                      case _ => None
                    }) match {
                      case Some(m) =>
                        sl ++ m
                      case _ => sl
                    }
                  case (sl, _) => sl
                }
              case (s, _) => s
            }
            .toList
            .map(_.toList)
      }
    }

    class NumSet(v1: Int, v2: Int, v3: Int, v4: Int) {
      val underlying: Set[Int] = Set(v1, v2, v3, v4)

      override def hashCode(): Int = underlying.hashCode()

      override def equals(obj: Any): Boolean = {
        obj match {
          case m: NumSet => m.underlying.equals(underlying)
        }
      }

      def toList: List[Int] = {
        List(v1, v2, v3, v4)
      }
    }
  }

  /*
  println(Solution.fourSum(
    Array(1,0,-1,0,-2,2), 0
  ))

  println(Solution.fourSum(
    Array(2,2,2,2,2), 8
  ))
  */

  //[[-3,-2,2,3],[-3,-1,1,3],[-3,0,0,3],[-3,0,1,2],[-2,-1,0,3],[-2,-1,1,2],[-2,0,0,2],[-1,0,0,1]]
  println(Solution.fourSum(
    Array(-3, -2, -1, 0, 0, 1, 2, 3), 0
  ))

}
