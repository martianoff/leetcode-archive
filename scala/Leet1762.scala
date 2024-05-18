object Leet1762 extends App {
  object Solution {
    def findBuildings(heights: Array[Int]): Array[Int] = {
      heights.zipWithIndex.foldRight(BuildingsAhead(buildings = List(), tallestBuilding = 0)) {
        case ((nextBuildingHeight, buildingIndex), BuildingsAhead(buildings, tallestBuilding)) if nextBuildingHeight > tallestBuilding =>
          BuildingsAhead(buildingIndex :: buildings, nextBuildingHeight)
        case (_, ahead) => ahead
      }.buildings.toArray
    }

    case class BuildingsAhead(buildings: List[Int], tallestBuilding: Int)
  }
}
