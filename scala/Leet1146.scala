import scala.collection.mutable

object Leet1146 extends App {

  class SnapshotArray(_length: Int) {

    var currentVersion: Int = 0
    var storage = mutable.Map.empty[Int, mutable.TreeMap[Int, Int]]

    // O(log n)
    def set(index: Int, `val`: Int) {
      if (!storage.contains(index))
        storage += (index -> mutable.TreeMap())
      storage(index) += (currentVersion -> `val`)
    }

    // O(1)
    def snap(): Int = {
      currentVersion += 1
      currentVersion - 1
    }

    // O(log n)
    def get(index: Int, snap_id: Int): Int = {
      // search for key
      storage.get(index) match {
        // key has version history
        case Some(versionTree) => {
          // search for the last version <= required version
          versionTree.maxBefore(snap_id + 1) match {
            case Some((_, v)) => v
            case None => 0
          }
        }
        // key is unknown
        case None => 0
      }
    }

  }

  /**
   * ["SnapshotArray","set","set","snap","get","set","snap","set","set","get","get"]
   * [[3],[1,18],[1,4],[],[0,0],[0,20],[],[0,2],[1,1],[1,1],[1,0]]
   * */


}
