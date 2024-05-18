object Leet102 extends App {

  import scala.collection.mutable

  class TreeNode(_value: Int = 0, _left: TreeNode = null, _right: TreeNode = null) {
    var value: Int = _value
    var left: TreeNode = _left
    var right: TreeNode = _right
  }

  object Solution {
    def levelOrder(root: TreeNode): List[List[Int]] = {
      Option(root).map {
        n =>
          val levels = mutable.Map.empty[Int, List[Int]].withDefaultValue(List())
          traverse(n, levels)
          (0 until levels.size).map(l => levels(l)).toList
      }.getOrElse(List())
    }

    private def traverse(node: TreeNode, levels: mutable.Map[Int, List[Int]], level: Int = 0): Unit = {
      levels(level) = node.value :: levels(level)
      (Option(node.left), Option(node.right)) match {
        case (None, Some(r)) => traverse(r, levels, level + 1)
        case (Some(l), None) => traverse(l, levels, level + 1)
        case (Some(l), Some(r)) =>
          traverse(r, levels, level + 1)
          traverse(l, levels, level + 1)
        case _ =>
      }
    }
  }
}
