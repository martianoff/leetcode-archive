import scala.collection.mutable

object Leet1366 extends App {

  import scala.collection.mutable
  import scala.collection.immutable

  object Solution {

    def rankTeams(votes: Array[String]): String = {
      val allTeams = mutable.TreeSet.empty[Char]
      val votesForTeam = mutable.Map.empty[Int, mutable.Map[Char, Int]]
      votes.foreach {
        vote =>
          vote.zipWithIndex.foreach {
            case (team, position) =>
              allTeams += team
              if (!votesForTeam.contains(position)) {
                votesForTeam += (position -> mutable.Map(team -> 1))
              } else {
                if (!votesForTeam(position).contains(team)) {
                  votesForTeam(position) += (team -> 1)
                } else {
                  votesForTeam(position)(team) += 1
                }
              }
          }
      }
      println(allTeams)
      val votesForTeamSorted = Map.from(votesForTeam.map {
        case (position, teamVotes) =>
          (position, immutable.TreeMap.from(teamVotes.groupBy(_._2).map { case (votes, array) => (votes, immutable.TreeSet.from(array.map(_._1))) }))
      })
      // go from 0 to the end position looking for tie
      // if no tie output team
      // if tie compare them at position two
      println(votesForTeamSorted)
      findWinners(votesForTeamSorted = votesForTeamSorted, position = 0, totalPos = allTeams.size, restTeams = immutable.TreeSet.from(allTeams)).mkString("")
    }

    private def findWinners(votesForTeamSorted: Map[Int, immutable.TreeMap[Int, immutable.TreeSet[Char]]], position: Int, totalPos: Int, restTeams: immutable.TreeSet[Char] = immutable.TreeSet()): List[Char] = {
      if (position == totalPos) {
        return List.empty[Char]
      }
      votesForTeamSorted.get(position) match {
        case Some(orderedVotes) =>
          // get teams with max votes
          orderedVotes.last match {
            case (_, teams) => {
              // if more than option search deeper first and backtrack
              if (teams.size > 1) {
                val nextWinners = findWinners(
                  votesForTeamSorted = votesForTeamSorted,
                  position + 1,
                  totalPos = totalPos,
                  restTeams = restTeams)
                (restTeams -- nextWinners.toSet match {
                  case possibleTeams => possibleTeams.last
                }) :: nextWinners
              } else {
                // if just one option return it and remove from restTeams
                teams.head :: findWinners(
                  votesForTeamSorted = votesForTeamSorted,
                  position + 1,
                  totalPos = totalPos,
                  restTeams - teams.head)
              }
            }
          }
        case None => List()
      }
    }

  }

}
