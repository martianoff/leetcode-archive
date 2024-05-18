object Leet95 extends App {
  class TreeNode(_value: Int = 0, _left: TreeNode = null, _right: TreeNode = null) {
    var value: Int = _value
    var left: TreeNode = _left
    var right: TreeNode = _right
  }

  object Solution {
    def generateTrees(n: Int): List[TreeNode] = {
      generateBinaryTrees(from = 1, to = n)
    }

    def generateBinaryTrees(from: Int, to: Int): List[TreeNode] = {
      if (from > to) return List[TreeNode](null)
      if (from == to) return List(new TreeNode(_value = from))
      (from to to).toList.foldLeft(List.empty[TreeNode]) {
        case (aggr, v) => generateBinaryTrees(from = from, to = v - 1).flatMap {
          l =>
            generateBinaryTrees(from = v + 1, to = to).map {
              r =>
                new TreeNode(_value = v, _left = l, _right = r)
            }
        } ++ aggr
      }
    }
  }

  println(Solution.generateTrees(4))
}
