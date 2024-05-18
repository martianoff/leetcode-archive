object Leet305 extends App {

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

    def union(id1: Int, id2: Int): Node = {
      val node1 = findInSet(id1)
      val node2 = findInSet(id2)
      if (node1 != node2) {
        if (node1.weight > node2.weight) {
          // merge 2 into 1
          node2.parent = node1.id
          node1.weight += node2.weight
          node1
        } else {
          // merge 1 to 2
          node1.parent = node2.id
          node2.weight += node1.weight
          node2
        }
      } else {
        node1
      }
    }

    case class Node(id: Int, var parent: Int, var weight: Int = 1)
  }

  object Solution {
    def numIslands2(m: Int, n: Int, positions: Array[Array[Int]]): List[Int] = {
      val grid = Array.fill(m)(Array.fill(n)(false))
      val uf = new UnionFind()
      val directions = List((0, 1), (0, -1), (-1, 0), (1, 0))
      // fillout grid and connect blocks
      positions.foldLeft(List(0)) {
        case (numberOfIsland :: oldNumberOfIslands, Array(y, x)) if grid(y)(x) =>
          numberOfIsland :: numberOfIsland :: oldNumberOfIslands
        case (numberOfIsland :: oldNumberOfIslands, Array(y, x)) if !grid(y)(x) =>
          grid(y)(x) = true
          // find all connected islands
          val connectedIslands = directions.foldLeft(Set.empty[Int]) {
            case (connectedIslands, (yOff, xOff)) if y + yOff >= 0 &&
              y + yOff < m &&
              x + xOff >= 0 &&
              x + xOff < n &&
              grid(y + yOff)(x + xOff) =>
              connectedIslands + uf.findInSet(yxToInt(y + yOff, x + xOff)).id
            case (connectedIslands, _) =>
              connectedIslands
          }
          // connect islands together
          connectedIslands.foreach {
            id => uf.union(id, yxToInt(y, x))
          }
          // calculate number of islands
          // calculate number of islands
          (numberOfIsland - connectedIslands.size + 1) :: numberOfIsland :: oldNumberOfIslands
        case (_, _) => throw new Exception("invalid input")
      }.init.reverse
    }

    private def yxToInt(y: Int, x: Int): Int = {
      y * 100000 + x
    }
  }

}
