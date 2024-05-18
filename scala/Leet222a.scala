object Leet222a extends App {

  class TreeNode(_value: Int = 0, _left: TreeNode = null, _right: TreeNode = null) {
    var value: Int = _value
    var left: TreeNode = _left
    var right: TreeNode = _right
  }

  import scala.math._

  object Solution {
    def countNodes(root: TreeNode): Int = {
      // calculate depth and find the first right entrance
      Option(root).map {
        node =>
          val d = depth(node)
          if (d == 0) return 1

          // Last level nodes are enumerated from 0 to 2**d - 1 (left -> right).
          // Perform binary search to check how many nodes exist.
          val maxNumberOfNodes = pow(2, d).toInt - 1
          var left = 1
          var right = maxNumberOfNodes
          var pivot = 0
          while (left <= right) {
            pivot = left + (right - left) / 2
            if (exists(pivot, d, root))
              left = pivot + 1
            else
              right = pivot - 1
          }

          // The tree contains 2**d - 1 nodes on the first (d - 1) levels
          // and left nodes on the last level.
          return maxNumberOfNodes + left;
      }.getOrElse(0)
    }

    def exists(index: Int, depth: Int, startNode: TreeNode): Boolean = {
      var left = 0
      var right = pow(2, depth).toInt - 1
      var node = startNode

      var i = 0
      while (i < depth) {
        val pivot = left + (right - left) / 2
        if (index <= pivot) {
          node = node.left
          right = pivot
        } else {
          node = node.right
          left = pivot + 1
        }
        i += 1
      }
      Option(node).nonEmpty
    }

    def depth(node: TreeNode): Int = {
      Option(node).flatMap(n => Option(n.left)).map {
        leftNode => 1 + depth(leftNode)
      }.getOrElse(0)
    }
  }
}