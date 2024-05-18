object Leet152 extends App {

  object Solution {
    def maxProduct(nums: Array[Int]): Int = {
      // split array by chunks separated by 0 because we can't divide by 0
      nums.foldLeft(List(List.empty[Int])) {
        case (Nil, num) if num != 0 => (List(num) :: Nil)
        case (Nil, _) => (List() :: Nil)
        case (h :: t, num) if num != 0 => h match {
          case Nil => List(num) :: t
          case hs :: ts => (num :: hs :: ts) :: t
        }
        case (h :: t, _) => (List() :: h :: t)
      } match {
        case chunks =>
          // calculate max of each subarray using same idea is max subarray sum
          // note that if there is any "0" in nums it could be an answer
          chunks.foldLeft(if (chunks.size > 1) 0 else Int.MinValue) {
            case (max, subarr) =>
              val l = subarr.foldLeft(List.empty[Long]) {
                case (Nil, num) => List(num)
                case (lastProduct :: restProducts, num) => ((lastProduct * num) :: lastProduct :: restProducts)
              }
              val r = subarr.foldRight(List.empty[Long]) {
                  case (num, Nil) => List(num)
                  case (num, lastProduct :: restProducts) => ((lastProduct * num) :: lastProduct :: restProducts)
                }
                .reverse
              max max maxOfSubarr(l = l, r = r).toInt
          }
      }
    }

    def maxOfSubarr(l: List[Long], r: List[Long]): Long = {
      (l, r) match {
        case (lh :: lt, rh :: rt) if lt.nonEmpty && rt.nonEmpty =>
          lh max rh max lh / rh max rh / lh max maxOfSubarr(lt, rt)
        case (lh :: Nil, rh :: Nil) => lh
        case _ => 0
      }
    }
  }

}
