object Leet1168 extends App {

  import scala.collection.mutable
  import scala.util.chaining._

  object Solution {
    def minCostToSupplyWater(n: Int, wells: Array[Int], pipes: Array[Array[Int]]): Int = {
      val unionFind = new UnionFind()
      // add wells to edges to simplify the problem
      val wellsEdges = wells.zipWithIndex.map {
        // we use the same ID for all wells because we need to connect just 1 well in total
        // and we connect it to every house
        case (cost, id) => Array(10001, id + 1, cost)
      }
      // sort all edges ASC
      val allEdges = (wellsEdges ++ pipes).sortWith {
        case (Array(from1, to1, cost1), Array(from2, to2, cost2)) => cost1 < cost2
      }
      // Kruskal’s Minimum Spanning Tree algorithm
      allEdges.map {
        case Array(from, to, cost) =>
          unionFind.union(from, to) match {
            case true => cost
            case false => 0 // ignore connected components
          }
      }.sum
    }

    class UnionFind() {
      val storage = mutable.Map.empty[Int, Node]

      def union(id1: Int, id2: Int): Boolean = {
        val node1 = findInSet(id1)
        val node2 = findInSet(id2)
        if (node1 != node2) {
          if (node1.rank < node2.rank) {
            // merge node 1 to node 2
            node1.parentId = node2.id
            node2.rank += node1.rank
          } else {
            // merge node 2 to node 1
            node2.parentId = node1.id
            node1.rank += node2.rank
          }
          true
        } else {
          false
        }
      }

      // returns parent Node
      def findInSet(id: Int): Node = {
        val node = storage.get(id) match {
          case Some(node) => node
          case None => Node(id, id).tap { n =>
            storage += (id -> n)
          }
        }
        if (node.parentId != node.id) {
          // path compression
          findInSet(node.parentId).tap {
            parent => node.parentId = parent.id
          }
        } else {
          node
        }
      }

      case class Node(id: Int, var parentId: Int, var rank: Int = 1)
    }
  }
}
