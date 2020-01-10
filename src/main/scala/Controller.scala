import Main.getClass

import scala.io.Codec

class Controller {
  private var inverseIndex= new InverseIndex

  def search(query: String): List[String] = {
    println("Sucheingabe:"+query)
    val searchTokens=new Tokenizer("de",query).tokenize()
    val relSearchTokens=new Stopwords("de").remove(searchTokens)

    val searchThesaurus=relSearchTokens++ relSearchTokens.flatMap(q=>new Thesaurus("de").getSynsets(q))

    println("such Thesaurus:")
    println(searchThesaurus)
    println("Suche in Stammform")
    println(new Stemmer(searchThesaurus).stemm())

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
      // imagine this requires several lines

      val content=line
      val nr=numPattern.findFirstIn(line).toString.replaceAll("[^0-9]", "");
      inverseIndex.insert(nr,content)

    }
    val t1 = System.nanoTime()
    println("Elapsed time: " + (t1 - t0)/1e+6/1000  + "sek")
    this.inverseIndex=inverseIndex

  }
}
