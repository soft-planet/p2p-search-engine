
import cats.syntax.either._
import io.circe._, io.circe.parser._
import java.net.URLEncoder
class Thesaurus(var language:String) {
//https://alvinalexander.com/scala/how-to-write-scala-http-get-request-client-source-fromurl
  //https://www.openthesaurus.de/synonyme/search?q=klausur&format=application/json
  var baseform =true
  var substringMaxResults=3
  def getSynsets(word:String): Set[String] ={


    var url="https://www.openthesaurus.de/synonyme/search?q="+URLEncoder.encode(word, "UTF-8")+"&format=application/json";

    if(baseform)url+="&baseform=true"

    url+="&substringMaxResults="+this.substringMaxResults

    val rawJson=scala.io.Source.fromURL(url).mkString

    val json : Json= parse(rawJson).getOrElse(Json.Null)
    val cursor: HCursor = json.hcursor
    val terms = cursor.downField("synsets").downArray.downField("terms").downArray
    var current =  terms
    var result:List[String]=List()

   while(current.right.get[String]("term").isRight)
   {

    result= result ++ List(current.right.get[String]("term").getOrElse(""))
    current=current.right
   }

  result.toSet

  }
}
