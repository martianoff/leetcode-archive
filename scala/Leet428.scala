object Leet428 extends App {

  class Node(var _value: Int) {
    var value: Int = _value
    var children: List[Node] = List()
  }

  import scala.util.chaining._
  import scala.collection.mutable

  class Codec {

    //value|len|childrenArray
    //childrenArray:
    //len1|...serialized1,len2|...serialized2
    // Encodes a tree to a single string.
    def serialize(root: Node): String = {
      if (root == null) return ""
      root.value + "|" + root.children.map {
          children =>
            serialize(children).pipe {
              serialized => serialized.length + "|" + serialized
            }
        }
        .pipe(_.mkString(","))
        .pipe {
          serialized => serialized.length + "|" + serialized
        }
    }

    // Decodes your encoded data to tree.
    def deserialize(data: String): Node = {
      if (data == "") return null
      var pointer = 0
      val buf = new mutable.StringBuilder
      val tokens = mutable.ListBuffer.empty[String]
      while (pointer < data.length) {
        data(pointer) match {
          case '|' =>
            tokens.append(buf.toString)
            buf.clear
            if (tokens.size == 2) {
              tokens.append(data.substring(pointer + 1))
              pointer += tokens(1).toInt
            }
          case _ =>
            buf.append(data(pointer))
        }
        pointer += 1
      }
      (new Node(tokens.head.toInt)).tap {
        n => n.children = deserializeList(tokens(2))
      }
    }

    //len1|...serialized1,len2|...serialized2
    private def deserializeList(data: String): List[Node] = {
      var pointer = 0
      val buf = new mutable.StringBuilder
      val tokens = mutable.ListBuffer.empty[String]
      while (pointer < data.length) {
        data(pointer) match {
          case ',' =>
          case '|' =>
            tokens.append(buf.toString)
            buf.clear
            if (tokens.size % 2 == 1) {
              val blockLength = tokens.last.toInt
              tokens.append(data.substring(pointer + 1, pointer + 1 + blockLength))
              pointer += blockLength
            }
          case _ =>
            buf.append(data(pointer))
        }
        pointer += 1
      }
      (1 until tokens.length by 2).collect {
        case tknIndex =>
          deserialize(tokens(tknIndex))
      }.toList
    }
  }
}
