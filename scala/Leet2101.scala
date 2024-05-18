object Leet2101 extends App {

  import scala.collection.mutable

  object Solution {
    def distance(p1: Point, p2: Point): Double = {
      math.sqrt(
        (p2.x.toDouble - p1.x.toDouble) * (p2.x.toDouble - p1.x.toDouble) +
          (p2.y.toDouble - p1.y.toDouble) * (p2.y.toDouble - p1.y.toDouble)
      )
    }

    def maximumDetonation(bombs: Array[Array[Int]]): Int = {
      val bombNeighbours = mutable.Map.empty[Bomb, Set[Bomb]].withDefaultValue(Set.empty[Bomb])
      val bombsObj = bombs.zipWithIndex.map { case (Array(x, y, r), i) => Bomb(i, Point(x, y), r) }
      // build neighbor directed connection graph
      bombsObj.foreach {
        bomb =>
          // optimized loop to avoid double calculations
          (bomb.index + 1 until bombs.length).foreach {
            otherBombIndex =>
              val otherBomb = bombsObj(otherBombIndex)
              val d = distance(bomb.point, otherBomb.point)
              if (d <= bomb.radius.toDouble) {
                bombNeighbours(bomb) += otherBomb
              }
              if (d <= otherBomb.radius.toDouble) {
                bombNeighbours(otherBomb) += bomb
              }
          }
      }
      // find how many bombs will be exposed as chaining reaction
      val totalBombedFromBomb = bombNeighbours.map {
        case (bomb, _) =>
          1 + dfs(bomb, bombNeighbours, mutable.Set(bomb))
      }
      // we can always bomb at least 1 bomb
      if (totalBombedFromBomb.isEmpty) {
        return 1
      }
      totalBombedFromBomb.max
    }

    def dfs(bomb: Bomb, allNeighbours: mutable.Map[Bomb, Set[Bomb]], visited: mutable.Set[Bomb]): Int = {
      allNeighbours.get(bomb) match {
        case Some(neighbours) =>
          neighbours.toList.collect {
            case b if !visited.contains(b) =>
              visited += b
              1 + dfs(b, allNeighbours, visited)
          }.sum
        case None =>
          0
      }
    }

    case class Point(x: Int, y: Int)

    case class Bomb(index: Int, point: Point, radius: Int)
  }


}
