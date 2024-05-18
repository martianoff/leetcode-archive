import scala.collection.mutable

object Leet2096 extends App {

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

  case class PathItem(value: Int, direction: Char)

  case class TreeNodeWithPath(treeNode: TreeNode, fromDirection: Char, backPath: List[PathItem])

  object Solution {

    var startPath: List[PathItem] = _
    var destPath: List[PathItem] = _

    def getDirections(root: TreeNode, startValue: Int, destValue: Int): String = {
      // find common
      startPath = null
      destPath = null
      val traverseQueue = new mutable.Queue[TreeNodeWithPath]
      traverseQueue.enqueue(TreeNodeWithPath(treeNode = root, backPath = List(), fromDirection = ' '))
      while (traverseQueue.nonEmpty) {
        traverseQueue.dequeue() match {
          case null =>
          case node =>
            if (node.treeNode.value == startValue) {
              startPath = PathItem(node.treeNode.value, node.fromDirection) :: node.backPath
            }
            if (node.treeNode.value == destValue) {
              destPath = PathItem(node.treeNode.value, node.fromDirection) :: node.backPath
            }
            // traverse if not found
            if ((startPath == null) || (destPath == null)) {
              val nodePath = PathItem(node.treeNode.value, node.fromDirection)
              node.treeNode match {
                case BinaryTree(Some(l), None) => traverseQueue.enqueue(TreeNodeWithPath(treeNode = l, fromDirection = 'L', backPath = nodePath :: node.backPath))
                case BinaryTree(None, Some(r)) => traverseQueue.enqueue(TreeNodeWithPath(treeNode = r, fromDirection = 'R', backPath = nodePath :: node.backPath))
                case BinaryTree(Some(l), Some(r)) =>
                  traverseQueue.enqueue(TreeNodeWithPath(treeNode = l, fromDirection = 'L', backPath = nodePath :: node.backPath))
                  traverseQueue.enqueue(TreeNodeWithPath(treeNode = r, fromDirection = 'R', backPath = nodePath :: node.backPath))
                case _ =>
              }
            }
        }
      }
      // find common
      val common = startPath.map(_.value).intersect(destPath.map(_.value)).head
      // print real path
      println(startPath.span(_.value != common)._1.map(_.value) ++ List(common) ++ destPath.span(_.value != common)._1.reverse.map(_.value))
      // build directions
      startPath.span(_.value != common)._1.map(_ => "U").mkString("") + destPath.span(_.value != common)._1.reverse.map(_.direction).filterNot(_ == ' ').mkString("")
    }
  }

  object BinaryTree {
    def unapply(n: TreeNode): Option[(Option[TreeNode], Option[TreeNode])] = {
      Some(
        Option(n.left),
        Option(n.right)
      )
    }
  }

  println(Solution.getDirections(
    tree, 3, 6))
  println(Solution.getDirections(
    tree, 6, 3))
  println(Solution.getDirections(
    tree, 6, 4))
  println(Solution.getDirections(
    tree, 4, 6))
  println(Solution.getDirections(
    tree, 1, 3))
  println(Solution.getDirections(
    tree, 3, 1))
}
