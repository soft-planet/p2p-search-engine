import java.io.{FileInputStream, FileOutputStream, ObjectInputStream, ObjectOutputStream}

import Main.getClass

import scala.io.Codec

class Controller {
  private var inverseIndex= new InverseIndex

  def search(query: String): List[String] = {

    val searchTokens=new Tokenizer("de",query).tokenize()
    val relSearchTokens=new Stopwords("de").remove(searchTokens)
    val searchThesaurus=relSearchTokens++ relSearchTokens.flatMap(q=>new Thesaurus("de").getSynsets(q))
    new Stemmer(searchThesaurus).stemm().flatMap(s=>this.inverseIndex.inverseIndex(s)).toList

  }

  def createInverseIndexCSV(file:String)={
    val t0 = System.nanoTime()

    val CR = 0x0D.toChar
    val LF = 0x0A.toChar
    val CRLF = ""+CR+ CR + LF
    val inverseIndex= new InverseIndex



    val src = scala.io.Source.fromInputStream( getClass.getResourceAsStream(file ) )(Codec("ISO-8859-1"))
    val iter = src.mkString.split(CRLF)
    val numPattern = "Nr.[ ]{0,1}[0-9]{2,}".r

    for (line <- iter) {


      val content=line
      val nr=numPattern.findFirstIn(line).toString.replaceAll("[^0-9]", "");
      inverseIndex.insert(nr,content)

    }
    val t1 = System.nanoTime()
    println("Elapsed time: " + (t1 - t0)/1e+6/1000  + "sek")
    this.inverseIndex=inverseIndex


  }

  def saveInverseIndex(file:String)={

      val oos = {
        new ObjectOutputStream(new FileOutputStream(file))
      }
      try {
         oos.writeObject(this.inverseIndex.inverseIndex)
      } finally {
        oos.close()
      }

  }

  def loadInverseIndex(file:String) = {
    val ois = new ObjectInputStream(new FileInputStream(file))
    try {
      this.inverseIndex.inverseIndex = ois.readObject.asInstanceOf[Map[String,Set[String]]]

    } finally {
      ois.close()
    }
  }
}
