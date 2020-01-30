import java.io.{FileInputStream, FileOutputStream, ObjectInputStream, ObjectOutputStream}

import de.htwb.wissrep.index.InvertedIndex

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

class Controller {
  private var invertedIndex : InvertedIndex[String, String, Any] = InvertedIndex()

  def search(query: String): List[String] = {
    println("Suchanfrage: " + query)
    val searchTokens=new Tokenizer("de",query).tokenize()
    val relSearchTokens=new Stopwords("de").remove(searchTokens)
    new Stemmer(relSearchTokens).stemm().map(x =>  x).flatMap(s=>this.invertedIndex(s).map(_._1.toString())).toList

  }

  private def transform(text: String) : Iterable[String]= {
    val tokens = new Tokenizer("german", text).tokenize();
    val stemms = new Stemmer(tokens).stemm()
    val result = new Stopwords("german").remove(stemms)
    result
  }

  
  def createInverseIndexJSON(file:String)={

    val t0 = System.nanoTime()

    val input = new FileInputStream(file)

    val content = read[BerichtJson](input)

    println("Read after: " + (System.nanoTime() - t0)/1e+6/1000  + "sec")

    val indexContent = content.content.mapValues(berichts => {
      val bericht = berichts.headOption.getOrElse(Bericht())
      val berichtContent = Iterable(bericht.content,
                                    bericht.title,
                                    bericht.number,
                                    bericht.date) 
                            .concat(bericht.bezirke)
      val berichtTokens = berichtContent.flatMap(transform)
                                        .map((_, null))
      berichtTokens.toSet
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
         oos.writeObject(this.invertedIndex.index)
      } finally {
        oos.close()
      }

  }

  def loadInverseIndex(file:String) = {
    val ois = new ObjectInputStream(new FileInputStream(file))
    try {
      val index = ois.readObject.asInstanceOf[Map[String, Set[(String, Any)]]]
      this.invertedIndex = InvertedIndex(index)
    } finally {
      ois.close()
    }
  }
}
