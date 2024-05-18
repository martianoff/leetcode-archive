object Leet426 extends App {

  /**
   * Definition for a Node.
   * class Node(var _value: Int) {
   * var value: Int = _value
   * var left: Node = null
   * var right: Node = null
   * }
   */

  import scala.collection.mutable

  class Node(var _value: Int) {
    var value: Int = _value
    var left: Node = null
    var right: Node = null
  }

  object Solution {
    def treeToDoublyList(root: Node): Node = {
      val stack = mutable.Stack.empty[(Node, Boolean)]
      stack.push((root, false))
      var tail: Node = null
      var second: Node = null
      var head: Node = root
      while (stack.nonEmpty) {
        stack.pop() match {
          case (BinaryTree(None, parent, Some(right)), visited) if visited == false =>
            stack.push((parent, true))
            stack.push((right, false))
          case (BinaryTree(Some(left), parent, None), visited) if visited == false =>
            stack.push((left, false))
            stack.push((parent, true))
          case (BinaryTree(Some(left), parent, Some(right)), visited) if visited == false =>
            stack.push((left, false))
            stack.push((parent, true))
            stack.push((right, false))
          case (BinaryTree(_, parent, _), _) =>
            // either already visited or no child
            if (tail == null) {
              tail = parent
              head = parent
              second = parent
            } else {
              second = head
              head = parent
              second.left = head
              head.right = second
            }
          case _ =>
        }
      }
      // connect head and tail
      if (tail != null) {
        head.left = tail
        tail.right = head
      }
      head
    }
  }

  object BinaryTree {
    def unapply(n: Node): Option[(Option[Node], Node, Option[Node])] = {
      n match {
        case null => None
        case _ => Some((Option(n.left), n, Option(n.right)))
      }
    }
  }
}
