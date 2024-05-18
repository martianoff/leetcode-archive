import scala.collection.mutable

object Leet158 extends App {
  class Reader4 {
    def read4(buf: Array[Char]): Int = {
      0
    }
  }

  class Solution extends Reader4 {
    val pending = mutable.Queue.empty[Char]
    val buf4 = Array.fill(4)('_')

    /**
     * @param buf Destination buffer
     * @param n   Number of characters to read
     * @return The number of actual characters read
     */
    def read(buf: Array[Char], n: Int): Int = {
      var pointer = 0
      var bytes = 0
      do {
        // release pending data
        while (pending.nonEmpty && pointer < n) {
          buf(pointer) = pending.dequeue()
          pointer += 1
        }
        // read extra data
        if (pointer < n) {
          bytes = read4(buf4)
          (0 until bytes).foreach {
            i =>
              pending.enqueue(buf4(i))
          }
        }
      } while (pointer < n && bytes != 0)
      pointer
    }
  }
}