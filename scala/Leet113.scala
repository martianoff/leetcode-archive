import scala.collection.immutable.{AbstractSeq, LinearSeq}

object Leet113 extends App {
  val tree = new TreeNode(_value = 5,
    _left = new TreeNode(_value = 1,
      _left = new TreeNode(_value = 3,
        _left = null,
        _right = null),
      _right = null),
    _right = new TreeNode(_value = 2,
      _left = new TreeNode(_value = 6,
        _left = null,
        _right = null),
      _right = new TreeNode(_value = 4,
        _left = null,
        _right = null)))

  class TreeNode(_value: Int = 0, _left: TreeNode = null, _right: TreeNode = null) {
    var value: Int = _value
    var left: TreeNode = _left
    var right: TreeNode = _right
  }

  object Solution {
    def pathSum(root: TreeNode, targetSum: Int): List[List[Int]] = {
      root match {
        // the bottom
        case TreeNode(None, None) if targetSum == root.value =>
          List(List(root.value))
        // the middle
        case TreeNode(lOpt, rOpt) =>
          // filter possible paths
          List(lOpt, rOpt).collect { case Some(node) => node }.map(
            node => pathSum(node, targetSum - root.value)
              .map(root.value :: _)
          ) match {
            case Nil => List.empty[List[Int]]
            case list => list.reduceLeft(_ ++ _)
          }
        // dead end
        case _ => List.empty[List[Int]]
      }
    }
  }

  object TreeNode {
    def unapply(node: TreeNode): Option[(Option[TreeNode], Option[TreeNode])] = {
      Option(node).map(node => (Option(node.left), Option(node.right)))
    }
  }

  println(Solution.pathSum(tree, 6))
}
