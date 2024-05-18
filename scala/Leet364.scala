object Leet364 extends App {

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
    def depthSumInverse(nestedList: List[NestedInteger]): Int = {
      weight(
        maxDepth = maxDepth(nestedList = nestedList),
        nestedList = nestedList
      )
    }

    def weight(depth: Int = 0, maxDepth: Int, nestedList: List[NestedInteger]): Int = {
      nestedList.collect {
        case v if v.isInteger => v.getInteger * (maxDepth - depth + 1)
        case v if !v.isInteger =>
          weight(depth = depth + 1, maxDepth = maxDepth, nestedList = v.getList.toList)
      } match {
        case someWeights if someWeights.nonEmpty => someWeights.sum
        case _ => 0
      }
    }

    def maxDepth(depth: Int = 0, nestedList: List[NestedInteger]): Int = {
      nestedList.collect {
        case v if !v.isInteger =>
          maxDepth(depth = depth + 1, nestedList = v.getList.toList)
      } match {
        case someDepths if someDepths.nonEmpty => someDepths.max
        case _ => depth
      }
    }
  }
}
