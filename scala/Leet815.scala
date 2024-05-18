
object Leet815 extends App {

  import scala.collection.mutable

  object Solution {
    def numBusesToDestination(routes: Array[Array[Int]], source: Int, target: Int): Int = {
      if (source == target)
        return 0
      val stopToRoutes = mutable.Map.empty[Int, Set[Int]].withDefaultValue(Set())
      val routeIdToRoute = mutable.Map.empty[Int, Route]
      routes.zipWithIndex.foreach {
        case (stops, routeId) =>
          routeIdToRoute += (routeId -> Route(routeId = routeId, stops = stops.toSet))
          stops.foreach {
            stopId =>
              stopToRoutes(stopId) = stopToRoutes(stopId) + routeId
          }
      }
      if (stopToRoutes(target).isEmpty)
        return -1
      // connect routes
      stopToRoutes.foreach {
        case (stopId, routeIds) if routeIds.size > 1 =>
          routeIds.map(routeIdToRoute(_)).foreach {
            route =>
              // aggregate connections
              routeIdToRoute(route.routeId) =
                route.copy(connectedRoutesIds = route.connectedRoutesIds ++ (routeIds - route.routeId))
          }
        case _ =>
      }
      val visitedRouteIds = mutable.Set.empty[Int]
      val search = mutable.Queue.empty[(Int, Int)]
      // start with routes containing starting point
      stopToRoutes(source).foreach {
        routeId =>
          visitedRouteIds += routeId
          search.enqueue((routeId, 1))
      }
      // do BFS
      while (search.nonEmpty) {
        search.dequeue() match {
          case (routeId, hops) =>
            val route = routeIdToRoute(routeId)
            if (route.stops.contains(target)) {
              return hops
            }
            route.connectedRoutesIds.foreach {
              case connectedRouteId if !visitedRouteIds.contains(connectedRouteId) =>
                visitedRouteIds += connectedRouteId
                search.enqueue((connectedRouteId, hops + 1))
              case _ =>
            }
        }
      }
      -1
    }

    case class Route(routeId: Int, stops: Set[Int], connectedRoutesIds: Set[Int] = Set())
  }

}