object Leet271 extends App {

  import java.util.Base64

  class Codec {

    val encoder = Base64.getEncoder
    val decoder = Base64.getDecoder
    val Separator = ':'

    // Encodes a list of strings to a single string.
    def encode(strs: List[String]): String = {
      strs.map {
        case s if s != "" => encoder.encodeToString(s.getBytes)
        case s => "_"
      }.mkString(Separator.toString)
    }

    // Decodes a single string to a list of strings.
    def decode(s: String): List[String] = {
      s.split(Separator).map {
        case s if s != "_" => new String(decoder.decode(s))
        case s => ""
      }.toList
    }
  }

}
