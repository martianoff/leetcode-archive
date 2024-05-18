object Leet323 extends App {

  import scala.collection.mutable
  import scala.util.chaining._

  class UnionFind() {
    private val storage = mutable.Map.empty[Int, Node]

    def findInSet(id: Int): Node = {
      val node = storage.get(id) match {
        case Some(n) => n
        case None => Node(id = id, parent = id).tap {
          n => storage += (id -> n)
        }
      }
      if (node.id != node.parent) {
        findInSet(node.parent)
          // path compression
          .tap { parent =>
            node.parent = parent.id
          }
      } else {
        node
      }
    }

    def union(id1: Int, id2: Int): Unit = {
      val node1 = findInSet(id1)
      val node2 = findInSet(id2)
      if (node1 != node2) {
        if (node1.weight > node2.weight) {
          // merge 2 into 1
          node2.parent = node1.id
          node1.weight += node2.weight
        } else {
          // merge 1 to 2
          node1.parent = node2.id
          node2.weight += node1.weight
        }
      }
    }

    case class Node(id: Int, var parent: Int, var weight: Int = 1)
  }

  object Solution {
    def countComponents(n: Int, edges: Array[Array[Int]]): Int = {
      val uf = new UnionFind()
      edges.foreach {
        case Array(a, b) => uf.union(a, b)
        case _ => throw new Exception("invalid input")
      }
      (0 until n).foldLeft(Set.empty[Int]) {
        case (parents, id) => parents + uf.findInSet(id).parent
      }.size
    }
  }

}
