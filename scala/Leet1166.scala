object Leet1166 extends App {

  import scala.collection.mutable
  import scala.util.chaining._

  class FileSystem() {

    val Separator = "/"
    val root = Node(name = Separator, value = 0)

    def createPath(path: String, value: Int): Boolean = {
      getPathComponents(path).pipe {
        pathComponents =>
          pathComponents.init.foldLeft(root) {
              case (node, folder) =>
                node.getFileByName(folder) match {
                  case None =>
                    return false
                  case Some(n) =>
                    n
                }
            }
            .addNode(Node(name = pathComponents.last, value = value))
      }
    }

    def get(path: String): Int = {
      getPathComponents(path).foldLeft(root) {
        case (node, folder) =>
          node.getFileByName(folder) match {
            case None =>
              return -1
            case Some(n) =>
              n
          }
      }.value
    }

    private def getPathComponents(path: String) = {
      path.stripPrefix(Separator).split(Separator)
    }

  }

  case class Node(name: String, value: Int, children: mutable.Map[String, Node] = mutable.Map.empty[String, Node]) {
    def getFileByName(name: String): Option[Node] = {
      children.get(name)
    }

    def addNode(node: Node): Boolean = {
      children.get(node.name) match {
        case None =>
          children += (node.name -> node)
          true
        case Some(n) =>
          false
      }
    }
  }

  /**
   * Your FileSystem object will be instantiated and called as such:
   * var obj = new FileSystem()
   * var param_1 = obj.createPath(path,value)
   * var param_2 = obj.get(path)
   */


}
