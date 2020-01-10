import java.io.InputStream

import Main.getClass

import scala.io.Codec

class Stopwords(var language:String) {
  def remove(words:Set[String])={

    val stream: InputStream = getClass.getResourceAsStream("stopwords-de.txt")
    val stopWords = scala.io.Source.fromInputStream( stream )(Codec("ISO-8859-1")).getLines().toSet

    words diff  stopWords
  }
}
