object Leet1235 extends App {

  import scala.collection.mutable
  import scala.collection.immutable
  import scala.util.chaining._

  object Solution {
    def jobScheduling(startTime: Array[Int], endTime: Array[Int], profit: Array[Int]): Int = {
      val storage = mutable.TreeMap.empty[Int, List[Job]].withDefaultValue(List())
      startTime.zipWithIndex.foreach {
        case (startTime, id) =>
          storage(startTime) = Job(startTime, endTime(id), profit(id)) :: storage(startTime)
      }
      val cache = mutable.Map.empty[(Int, Option[Int]), Int]
      dfs(storage.head._1, immutable.SortedMap.from(storage), cache)
    }

    private def dfs(startTime: Int, data: immutable.SortedMap[Int, List[Job]], cache: mutable.Map[(Int, Option[Int]), Int]): Int = {
      val cacheKey = (startTime, data.headOption.map(_._1))
      if (cache.contains(cacheKey)) {
        return cache(cacheKey)
      }
      (data.headOption match {
        case None => 0
        case Some((_, jobs)) =>
          // pick one of available jobs
          jobs.map {
            job => (job.profit + dfs(job.endTime, data.rangeFrom(job.endTime), cache))
          }.max max
            // skip all these jobs
            dfs(jobs.map(_.endTime).max, data.tail, cache)
      }).tap {
        r => cache += (cacheKey -> r)
      }
    }

    case class Job(startTime: Int, endTime: Int, profit: Int)
  }
}
