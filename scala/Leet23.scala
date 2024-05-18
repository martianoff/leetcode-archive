object Leet23 extends App {

  val ns = Solution.removeElement(arr, 3)
  var arr = Array(1, 2, 3, 3, 3, 4, 5)

  object Solution {
    def removeElement(nums: Array[Int], `val`: Int): Int = {
      var p: Int = 0
      nums.foreach(v => {
        if (v != `val`) {
          nums(p) = v
          p += 1
        }
      })
      p
    }
  }

  println(arr.take(ns).mkString(","))

}
