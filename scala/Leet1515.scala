

object Leet1515 extends App {

  object Solution {
    def getMinDistSum(positions: Array[Array[Int]]): Double = {
      val border = positions.foldLeft((100, 100, 0, 0)) {
        case ((left, bottom, right, top), Array(x, y)) =>
          (left min x, bottom min y, right max x, top max y)
      }
      var minDistance = Double.MaxValue
      var depotX: Double = 0
      var depotY: Double = 0
      var zoom: Double = 10.0
      var left: Double = border._1
      var bottom: Double = border._2
      var right: Double = border._3
      var top: Double = border._4
      while (zoom >= 0.00001) {
        var areaX = left
        while (areaX <= right) {
          var areaY = bottom
          while (areaY <= top) {
            // distance to the approximated area
            val distanceToArea = positions.foldLeft(0.0) {
              case (distance, Array(x, y)) =>
                distance + Math.sqrt((x - areaX) * (x - areaX) + (y - areaY) * (y - areaY))
            }
            // pick the best area
            if (distanceToArea < minDistance) {
              depotX = areaX
              depotY = areaY
              minDistance = distanceToArea
            }
            areaY += zoom
          }
          areaX += zoom
        }
        // adjust search area by including surrounding areas
        left = depotX - zoom
        bottom = depotY - zoom
        right = depotX + 2 * zoom
        top = depotY + 2 * zoom
        zoom /= 10
      }
      minDistance
    }
  }

}
