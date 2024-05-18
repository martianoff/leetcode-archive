object Leet71 extends App {
  object Solution {
    def simplifyPath(path: String): String = {
      "/" +
        // normalize format
        path.stripSuffix("/")
          .stripPrefix("/")
          .split("/")
          // deal with . and .. and extra /
          .foldLeft(List.empty[String]) {
            case (pathChunks, path) if path != "." && path != ".." && path != "" => path :: pathChunks
            case (pathChunks, path) if path == "." || path == "" => pathChunks
            case (_ :: otherChunks, _) => otherChunks
            case (pathChunks, _) => pathChunks
          }
          .reverse
          .mkString("/")
    }
  }
}
