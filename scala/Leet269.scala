object Leet269 extends App {

  import scala.collection.mutable

  object Solution {

    def alienOrder(words: Array[String]): String = {
      // build graph
      val graph = mutable.Map.empty[Char, Set[Char]]
      if (!extractGraph(words.map(_.toList), graph)) {
        return ""
      }
      // get a list of all nodes
      val allNodes = mutable.Set.empty[Char]
      words.foreach {
        word =>
          word.foreach {
            c => allNodes += c
          }
      }
      // count indegree
      val indegree = mutable.Map.empty[Char, Int]
      allNodes.foreach(node =>
        indegree(node) = 0)
      graph.foreach {
        case (_, toList) => toList.foreach {
          to => indegree(to) += 1
        }
      }
      // topographical search
      val queue = mutable.Queue.empty[Char]
      val result = mutable.Queue.empty[Char]
      indegree.foreach {
        case (node, v) if v == 0 =>
          queue.enqueue(node)
        case _ =>
      }
      while (queue.nonEmpty) {
        queue.dequeue() match {
          case node =>
            result.enqueue(node)
            if (graph.contains(node)) {
              // adjust indegree
              graph(node).foreach(connectedNode =>
                indegree(connectedNode) -= 1)
              // start new search
              graph(node).foreach {
                // add to search if indegree = 0
                case connectedNode if indegree(connectedNode) == 0 =>
                  queue.enqueue(connectedNode)
                // skip other nodes
                case _ =>
              }
            }
        }
      }
      if (result.size == allNodes.size) {
        result.mkString("")
      } else {
        ""
      }
    }

    def extractGraph(words: Array[List[Char]], graph: mutable.Map[Char, Set[Char]]): Boolean = {
      if (words.isEmpty || !words.exists(_.nonEmpty)) {
        return true
      }
      var prevOpt = Option.empty[Char]
      val newWordsList = words.collect {
          // required to capture ["abc","ab"]
          case Nil if prevOpt.nonEmpty => return false
          case Nil => ('_', Nil)
          case head :: rest =>
            if (!graph.contains(head)) {
              graph += (head -> Set())
            }
            prevOpt match {
              case Some(prev) if prev != head =>
                graph(prev) += head
              case _ =>
            }
            prevOpt = Some(head)
            (head, rest)
        }
        .groupBy(_._1)
        .collect { case (head, arr) => arr.map(_._2) }
      if (newWordsList.isEmpty) {
        return true
      }
      newWordsList.map {
          words =>
            extractGraph(
              words = words,
              graph = graph
            )
        }
        // required to capture ["abc","ab"]
        .reduceLeft(_ && _)
    }
  }

}
