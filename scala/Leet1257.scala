object Leet1257 extends App {

  import scala.collection.mutable

  object Solution {
    def findSmallestRegion(regions: List[List[String]], region1: String, region2: String): String = {
      if (region1 == region2) {
        return region1
      }
      // build children to parent graph
      val graph = mutable.Map.empty[String, String]
      regions.foreach {
        case head :: children => children.foreach {
          n => graph(n) = head
        }
        case _ => throw new Exception("invalid input")
      }
      // common ancestor
      val queue = mutable.Queue.empty[String]
      val visited = mutable.Set.from(List(region1, region2))
      queue.enqueue(region1)
      queue.enqueue(region2)
      while (queue.nonEmpty) {
        val region = queue.dequeue()
        graph.get(region) match {
          case Some(parent) if visited.contains(parent) =>
            return parent
          case Some(parent) =>
            visited += parent
            queue.enqueue(parent)
          case _ =>
        }
      }
      "unknown"
    }
  }
}
