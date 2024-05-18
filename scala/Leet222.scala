object Leet222 extends App {

  class TreeNode(_value: Int = 0, _left: TreeNode = null, _right: TreeNode = null) {
    var value: Int = _value
    var left: TreeNode = _left
    var right: TreeNode = _right
  }

  object Solution {
    def countNodes(root: TreeNode): Int = {
      Option(root).map(n =>
        1 +
          Option(n.left).map(countNodes).getOrElse(0) +
          Option(n.right).map(countNodes).getOrElse(0)
      ).getOrElse(0)
    }
  }
}