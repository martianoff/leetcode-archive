import scala.collection.mutable

object Leet143 extends App {

  class ListNode(_x: Int = 0, _next: ListNode = null) {
    val x: Int = _x
    var next: ListNode = _next
  }

  object Solution {
    def reorderList(head: ListNode): Unit = {
      val m = collection.mutable.Map.empty[Int, ListNode]
      var pos = 0
      var node = head
      while (node != null) {
        m(pos) = node
        pos = pos + 1
        node = node.next
      }
      pos = 0
      var normpos = 0
      node = head
      while (pos < m.size - 1) {
        if (pos % 2 == 0) {
          node.next = m(m.size - normpos - 1)
          normpos = normpos + 1
        } else {
          node.next = m(normpos)
        }
        pos = pos + 1
        node = node.next
      }
      node.next = null
    }
  }

  //println(Solution.reorderList(""))

}
