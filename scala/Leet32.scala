object Leet32 extends App {

  object Solution {

    import scala.collection.mutable

    def longestValidParentheses(s: String): Int = {
      val stack = mutable.Stack.from(Array(-1))
      s.zipWithIndex.foldLeft(0) {
        case (best, ('(', index)) =>
          stack.push(index)
          best
        case (best, (')', index)) =>
          // we always remove an extra scope because (() (_) > than _()
          stack.pop()
          if (stack.isEmpty) {
            // if sequence is invalid we start from this index
            stack.push(index)
            best
          } else {
            best max (index - stack.head)
          }
      }
    }
  }
}
