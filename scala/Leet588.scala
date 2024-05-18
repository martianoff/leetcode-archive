object Leet588 extends App {

  import scala.util.chaining._
  import scala.collection.mutable

  class FileSystem() {

    val Separator = "/"
    val root: Node = Node(Separator)

    def ls(path: String): List[String] = {
      path.stripPrefix(Separator).split(Separator).foldLeft(root) {
        case (parent, "") => parent
        case (parent, folder) =>
          parent.findChildNode(folder) match {
            case Some(n) => n
            case None => return List()
          }
      }.pipe {
        case node if node.isDir => node.nodes.toList.map { case (name, _) => name }
        case node => List(node.name)
      }
    }

    def mkdir(path: String) {
      path.stripPrefix(Separator).split(Separator).foldLeft(root) {
        case (parent, folder) =>
          parent.addChildNode(folder)
      }
    }

    def addContentToFile(filePath: String, content: String) {
      val pathChunks = filePath.stripPrefix(Separator).split(Separator)
      pathChunks.init.foldLeft(root) {
        case (parent, folder) =>
          parent.findChildNode(folder) match {
            case Some(n) => n
            case None => return
          }
      }.appendContent(pathChunks.last, content)
    }

    def readContentFromFile(filePath: String): String = {
      filePath.stripPrefix(Separator).split(Separator).foldLeft(root) {
        case (parent, folder) =>
          parent.findChildNode(folder) match {
            case Some(n) => n
            case None => return ""
          }
      }.value.getOrElse("")
    }

  }

  case class Node(
                   name: String,
                   value: Option[String] = None,
                   nodes: mutable.TreeMap[String, Node] = mutable.TreeMap.empty[String, Node]) {

    def isDir: Boolean = value.isEmpty

    def addChildNode(name: String): Node = {
      findChildNode(name) match {
        case Some(n) => n
        case None =>
          Node(name).tap {
            n => nodes += (name -> n)
          }
      }
    }

    def findChildNode(name: String): Option[Node] = {
      nodes.get(name)
    }

    def appendContent(name: String, value: String): Node = {
      findChildNode(name) match {
        case Some(n) =>
          n.copy(value = Some(n.value.get + value)).tap {
            newNode => nodes(name) = newNode
          }
        case None =>
          Node(name, Some(value)).tap {
            n => nodes += (name -> n)
          }
      }
    }
  }

}
