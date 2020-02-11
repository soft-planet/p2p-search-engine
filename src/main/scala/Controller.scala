import java.io.{FileInputStream, FileOutputStream, ObjectInputStream, ObjectOutputStream}

import de.htwb.wissrep.index.InvertedIndex
import de.htwb.wissrep.index.CosineSimilarityDocument

import Main.getClass

import scala.io.Codec

import upickle.default._
import upickle.default.{ReadWriter => RW, macroRW}
import upickle.implicits.key


import ujson.{Obj, Arr, Value}

final private case class BerichtJson(content: Map[String, List[Bericht]])
private object BerichtJson{
  def marshall(b: BerichtJson): Value = {
    Obj.from(b.content.mapValues(x => Arr(x.map(Bericht.marshall))))
  }
  def unmarshall(v : Value) : BerichtJson = {
    BerichtJson(v.obj.mapValues(_.arr.map(Bericht.unmarshall).toList).toMap)
  }
  implicit val rw: RW[BerichtJson] = readwriter[Value].bimap[BerichtJson](marshall, unmarshall)
}
private final case class Bericht( bezirke: List[String] = List(), 
                                  date: String = "",
                                  content: String = "",
                                  number: String = "",
                                  title: String = "")
private object Bericht{
  def marshall(b :Bericht): Value = {
    Obj(
        "Bezirke" -> Arr(b.bezirke),
        "Datum"   -> b.date,
        "Meldung" -> b.content,
        "Nummer"  -> b.number,
        "Titel"   -> b.title
    )
  }
  def unmarshall(v: Value): Bericht = {
    val b = v.obj
    Bericht(b("Bezirke").arr.map(_.str).toList,
            b("Datum").strOpt.getOrElse(""),
            b("Meldung").strOpt.getOrElse(""),
            b("Nummer").strOpt.getOrElse(""),
            b("Titel").strOpt.getOrElse("")
    )
  }
  implicit val rw: RW[Bericht] = readwriter[Value].bimap[Bericht](marshall, unmarshall)
}

final case class PolDocument(title: String, uri: String, tokenCounts: Map[String, Int], actualSize: Int) extends Serializable with CosineSimilarityDocument[String]{
  override def size = actualSize
  override def getTokenCounts = tokenCounts
}

object PolDocument{
  def apply(title: String, uri: String, tokenCounts: Map[String, Int]): PolDocument = 
    PolDocument(title, uri, tokenCounts, tokenCounts.values.reduceOption(_+_).getOrElse(0))
  
  def apply(title: String, uri: String, tokens: List[String]): PolDocument = 
    PolDocument(title, uri, tokens.groupBy(identity).view.mapValues(_.size).toMap)
  implicit val rw: RW[PolDocument] = macroRW
}

object Controller{
    def transform(text: String) : Iterable[String]= {
    val tokens = new Tokenizer("german", text).tokenize();
    val stemms = new Stemmer(tokens).stemm()
    val result = new Stopwords("german").remove(stemms)
    result
  }
}
class Controller {
  private var invertedIndex : InvertedIndex[PolDocument, String, Null] = InvertedIndex()

  def search(query: String): Map[String, List[PolDocument]] = {// [serach-word, documents]
    val searchTerms = Controller.transform(query)
    searchTerms.map(x => (x, this.invertedIndex(x).map(_._1).toList)).toMap
  }

  
  def createInverseIndexJSON(file:String)={

    val t0 = System.nanoTime()

    val input = new FileInputStream(file)

    val content = read[BerichtJson](input)

    println("Read after: " + (System.nanoTime() - t0)/1e+6/1000  + "sec")

    val indexContent = content.content.map(tuple => {
      val bericht = tuple._2.headOption.getOrElse(Bericht())
      val berichtContent = Iterable(bericht.content,
                                    bericht.title,
                                    bericht.number,
                                    bericht.date) 
                            .concat(bericht.bezirke)
      val berichtTokens = berichtContent.flatMap(Controller.transform)
                                        .map((_, null))
      val doc = PolDocument(bericht.title, tuple._1, berichtTokens.map(_._1).toList)
      (doc, berichtTokens.toSet)
    })

    println("Tokenized after: " + (System.nanoTime() - t0)/1e+6/1000  + "sec")

    invertedIndex = InvertedIndex(indexContent.toSeq: _*) 

    println("Indexed after: " + (System.nanoTime() - t0)/1e+6/1000  + "sec")

    println("Elapsed time: " + (System.nanoTime() - t0)/1e+6/1000  + "sec")

  }

  def saveInverseIndex(file:String)={

      val oos = {
        new ObjectOutputStream(new FileOutputStream(file))
      }
      try {
        val index = invertedIndex.index.view.mapValues(_.toList.map(x => (x._1.title, x._1.uri, x._1.tokenCounts, x._1.size))).toList
        oos.writeObject(index)
      } finally {
        oos.close()
      }

  }

  def loadInverseIndex(file:String) = {
    val ois = new ObjectInputStream(new FileInputStream(file))
    try {
      val rawIndex = ois.readObject.asInstanceOf[List[(String, List[(String, String, Map[String, Int], Int)])]]
      val index = rawIndex.map(y => (y._1, y._2.map(x => (PolDocument(x._1, x._2, x._3), null)).toSet)).toMap
      this.invertedIndex = InvertedIndex[PolDocument, String, Null](index)
    } finally {
      ois.close()
    }
  }

  def index: InvertedIndex[PolDocument, String, Null] = invertedIndex
}
