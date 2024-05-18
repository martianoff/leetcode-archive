object Leet1650 extends App {

  import scala.collection.mutable

  class Node(var _value: Int) {
    var value: Int = _value
    var left: Node = null
    var right: Node = null
    var parent: Node = null
  }

  object Solution {
    def lowestCommonAncestor(p: Node, q: Node): Node = {
      var node1 = Option(p)
      var node2 = Option(q)
      val parentsSet1 = mutable.Set.empty[Node]
      val parentsSet2 = mutable.Set.empty[Node]
      var common = Option.empty[Node]
      // move to the parent for both nodes at the same time
      do {
        (node1, node2) match {
          case (None, None) =>
          case (a, b) =>
            a match {
              // if node is in history it is our common node
              case Some(n1) if parentsSet2.contains(n1) =>
                common = Some(n1)
              // move to the parent and store history of nodes
              case Some(n1) =>
                parentsSet1 += n1
                node1 = Option(n1.parent)
              case None =>
            }
            b match {
              // if node is in history it is our common node
              case Some(n2) if parentsSet1.contains(n2) =>
                common = Some(n2)
              // move to the parent and store history of nodes
              case Some(n2) =>
                parentsSet2 += n2
                node2 = Option(n2.parent)
              case None =>
            }
        }
      } while (common.isEmpty)
      common.get
    }
  }

}
