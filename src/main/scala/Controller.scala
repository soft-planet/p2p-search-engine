import java.io.{FileInputStream, FileOutputStream, ObjectInputStream, ObjectOutputStream}

import de.htwb.wissrep.index.InvertedIndex

import Main.getClass

import scala.io.Codec

class Controller {
  private var invertedIndex : InvertedIndex[Int, String, Any] = InvertedIndex()

  def search(query: String): List[String] = {
    Console.err.println("Query: " + query)
    val searchTokens=new Tokenizer("de",query).tokenize()
    val relSearchTokens=new Stopwords("de").remove(searchTokens)
    val searchThesaurus=relSearchTokens++ relSearchTokens.flatMap(q=>new Thesaurus("de").getSynsets(q))
    new Stemmer(searchThesaurus).stemm().map(x => {println(x); x}).flatMap(s=>this.invertedIndex(s).map(_._1.toString())).toList

  }

  private def transform(text: String) : Iterable[String]= {
    val tokens = new Tokenizer("german", text).tokenize();
    val stemms = new Stemmer(tokens).stemm()
    val result = new Stopwords("german").remove(stemms)
    result
  }

  def createInverseIndexCSV(file:String)={
    val t0 = System.nanoTime()

    val CR = 0x0D.toChar
    val LF = 0x0A.toChar
    val CRLF = ""+CR+ CR + LF
    val inverseIndex= new InverseIndex



    val src = scala.io.Source.fromInputStream( getClass.getResourceAsStream(file ) )(Codec("ISO-8859-1"))
    val lines = src.mkString.split(CRLF)
    val numPattern = "Nr.[ ]{0,1}[0-9]{2,}".r

    val docFeatures = lines.map(line => { 
      val idString = numPattern.findFirstIn(line).toString.replaceAll("[^0-9]", "")
      val id = if(idString != "") Integer.parseInt(idString) else -1
      val content = transform(line).map((_, null))
      (id, content)
    })
    invertedIndex = InvertedIndex(docFeatures: _*) 

    val t1 = System.nanoTime()
    println("Elapsed time: " + (t1 - t0)/1e+6/1000  + "sek")
    this.invertedIndex=invertedIndex


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
      val index = ois.readObject.asInstanceOf[Map[String, Set[(Int, Any)]]]
      this.invertedIndex = InvertedIndex(index)
      Console.err.println(index)
    } finally {
      ois.close()
    }
  }
}
