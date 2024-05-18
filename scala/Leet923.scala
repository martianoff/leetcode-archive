object Leet923 extends App {

  import scala.collection.immutable

  object Solution {
    // for cases with 2 or 3 similar elements for example 2+2+1 or 3+3+3 we need to be able quickly
    // determine number of combinations, using intuition we can find formulas for number of combinations
    // based on frequency:
    //
    // 2 elements
    // [2,2]=1
    // [2,2,2]=1+2=3 (prev + freq-1)
    // [2,2,2,2]=3+3=6 (prev + freq-1)
    // [2,2,2,2,2]=4+3+3=10 (prev + freq-1)
    // [2,2,2,2,2,2]=5+4+3+3=15 (prev + freq-1)
    // or use formula (freq)*(freq-1)/2
    //
    // 3 elements
    // [2,2,2]=1
    // [2,2,2,2]=3+1=4 (prev + 2combo(freq-1))
    // [2,2,2,2,2]=6+4=10 (prev + 2combo(freq-1))
    // [2,2,2,2,2,2]=10+10=20 (prev + 2combo(freq-1))
    // or use formula (freq)*(freq-1)*(freq-2)/6
    //
    // precalculate frequencies for 3000 numbers based on above formulas
    val combinationsOfTwo: Map[Int, Long] = (2 to 3000).foldLeft((Map.empty[Int, Long], 0L)) {
      case ((m, lastF), f) => (lastF + (f.toLong - 1L)) match {
        case newFreq => (m + (f -> newFreq), newFreq)
      }
    }._1
    val combinationsOfThree: Map[Int, Long] = (3 to 3000).foldLeft((Map.empty[Int, Long], 0L)) {
      case ((m, lastF), f) => (lastF + combinationsOfTwo(f - 1)) match {
        case newFreq => (m + (f -> newFreq), newFreq)
      }
    }._1

    def threeSumMulti(arr: Array[Int], target: Int): Int = {
      // build map of frequencies
      // TreeMap helps us to minimize second iteration using range query
      val valuesFreq = immutable.TreeMap.from(arr.zipWithIndex.groupBy(_._1).map {
        case (v, list) => (v, list.length)
      })
      valuesFreq.foldLeft(0L) {
        case (total, (v1, freq1)) =>
          valuesFreq.range(v1, target - 2 * v1 + 1).foldLeft(0L) {
            case (total, (v2, freq2)) =>
              val v3 = target - v1 - v2
              if (v3 >= v2) {
                valuesFreq.get(v3) match {
                  case Some(freq3) =>
                    // all different
                    if (v1 < v2 && v2 < v3) {
                      total + freq1 * freq2 * freq3
                    } else if (v1 == v2 && v2 == v3 && freq1 >= 3) {
                      // all same
                      total + combinationsOfThree(freq1)
                    } else if (v1 == v2 && v2 != v3 && freq1 >= 2) {
                      // first two are the same
                      total + combinationsOfTwo(freq1) * freq3
                    } else if (v2 == v3 && v1 != v2 && freq2 >= 2) {
                      // second two are the same
                      total + combinationsOfTwo(freq2) * freq1
                    } else {
                      total
                    }
                  case _ => total
                }
              }
              else total
          } + total
      } match {
        case v => (v % 1000000007L).toInt
      }
    }
  }
  //(1, 2, 5) occurs 8 times;
  //(1, 3, 4) occurs 8 times;
  //(2, 2, 4) occurs 2 times;
  //(2, 3, 3) occurs 2 times.

  //20
  //println(Solution.threeSumMulti(Array(1,1,2,2,3,3,4,4,5,5),8))

  //We choose one 1 from [1,1] in 2 ways,
  //and two 2s from [2,2,2,2] in 6 ways.
  //2 elements
  //[2,2]=1
  //[2,2,2]=1+2=3 (prev + len(prev))
  //[2,2,2,2]=3+3=6 (prev + len(prev))
  //[2,2,2,2,2]=4+3+3=10 (prev + len(prev))
  //[2,2,2,2,2,2]=5+4+3+3=15 (prev + len(prev))
  //3 elements
  //[2,2,2]=1
  //[2,2,2,2]=3+1=4 (prev + 2combo(len-1))
  //[2,2,2,2,2]=6+4=10 (prev + 2combo(len-1))
  //[2,2,2,2,2,2]=10+10=20 (prev + 2combo(len-1))
  //permutations
  //P(n,r)=n!(n−r)! for 2 from 4 = 4!/2!=(1*2*3*4)/(1*2)=12
  //12
  //println(Solution.threeSumMulti(Array(1,1,2,2,2,2), 5))
  //println(Solution.threeSumMulti(Array(0,0,0), 0))
  //println(Solution.threeSumMulti(Array(2,1,3), 6))
  //12
  println(Solution.threeSumMulti(Array(3, 3, 0, 0, 3, 2, 2, 3), 6))

}
