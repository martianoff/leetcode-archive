object Leet339 extends App {

  trait NestedInteger {
    // Return true if this NestedInteger holds a single integer, rather than a nested list.
    def isInteger: Boolean

    // Return the single integer that this NestedInteger holds, if it holds a single integer.
    def getInteger: Int

    // Set this NestedInteger to hold a single integer.
    def setInteger(i: Int): Unit

    // Return the nested list that this NestedInteger holds, if it holds a nested list.
    def getList: Array[NestedInteger]

    // Set this NestedInteger to hold a nested list and adds a nested integer to it.
    def add(ni: NestedInteger): Unit
  }

  object Solution {
    def depthSum(nestedList: List[NestedInteger]): Int = {
      ds(nestedList.toArray, 1)
    }

    def ds(n: Array[NestedInteger], depth: Int): Int = {
      n.map {
        case n if n.isInteger => depth * n.getInteger
        case n => ds(n.getList, depth + 1)
      }.sum
    }
  }

}
