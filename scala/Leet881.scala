object Leet881 extends App {

  object Solution {
    def numRescueBoats(people: Array[Int], limit: Int): Int = {
      people.sortInPlace
      var lowest = 0
      var highest = people.length - 1
      var boats = 0
      while (lowest <= highest) {
        boats += 1
        // we always push highest first
        // and lowest if we have enough additional space
        if (people(lowest) + people(highest) <= limit) {
          // 2 people: lowest + highest
          lowest += 1
        }
        highest -= 1
      }
      boats
    }
  }


}
