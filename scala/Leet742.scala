object Leet742 extends App {

  import scala.collection.mutable

  class TreeNode(_value: Int = 0, _left: TreeNode = null, _right: TreeNode = null) {
    var value: Int = _value
    var left: TreeNode = _left
    var right: TreeNode = _right
  }

  object Solution {
    def findClosestLeaf(root: TreeNode, k: Int): Int = {
      findKNode(root, k)
        .flatMap(findLeafNode)
        .map(_.value)
        .getOrElse(0)
    }

    def findLeafNode(node: TreeNodeWithParent): Option[TreeNode] = {
      val queue = mutable.Queue.from(List(node))
      val visited = mutable.Set.empty[Int]
      while (queue.nonEmpty) {
        val next = queue.dequeue()
        visited += next.node.map(_.value).getOrElse(0)
        if (next.node.map(_.value).contains(3)) {
          println(next.parent.flatMap(_.node).map(_.value))
        }
        next match {
          case TreeNodeWithParent(Some(node), _) if node.left == null && node.right == null =>
            return Some(node)
          case TreeNodeWithParent(Some(node), parentOpt) =>
            parentOpt match {
              case Some(parent) if !visited.contains(parent.node.map(_.value).getOrElse(0)) =>
                queue.enqueue(
                  TreeNodeWithParent(node = parent.node, parent = parent.parent)
                )
              case _ =>
            }
            List(Option(node.left), Option(node.right))
              .filter(nOpt => !visited.contains(nOpt.map(_.value).getOrElse(0)))
              .foreach {
                nOpt =>
                  queue.enqueue(
                    TreeNodeWithParent(node = nOpt, parent = Some(TreeNodeWithParent(Some(node), parentOpt)))
                  )
              }
          case TreeNodeWithParent(None, _) =>
        }
      }
      None
    }

    def findKNode(root: TreeNode, k: Int): Option[TreeNodeWithParent] = {
      val queue = mutable.Queue.from(List(
        TreeNodeWithParent(node = Option(root), parent = Option.empty[TreeNodeWithParent])
      ))
      while (queue.nonEmpty) {
        queue.dequeue() match {
          case TreeNodeWithParent(Some(node), parentOpt) if node.value == k =>
            return Some(TreeNodeWithParent(Some(node), parentOpt))
          case TreeNodeWithParent(Some(node), parentOpt) =>
            List(Option(node.left), Option(node.right)).foreach {
              nOpt =>
                queue.enqueue(TreeNodeWithParent(
                  node = nOpt,
                  parent = Some(TreeNodeWithParent(Some(node), parentOpt))))
            }
          case TreeNodeWithParent(None, _) =>
        }
      }
      None
    }

    case class TreeNodeWithParent(node: Option[TreeNode], parent: Option[TreeNodeWithParent])
  }

}
