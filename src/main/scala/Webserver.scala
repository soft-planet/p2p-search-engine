import java.io.InputStream

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.model._
import akka.http.scaladsl.server.Directives._
import akka.stream.ActorMaterializer
import akka.http.scaladsl.unmarshalling.Unmarshal

import scala.io.{Codec, StdIn}

import upickle.default._
import scala.concurrent.Await
import scala.concurrent.duration._
import akka.stream.Materializer
import scala.concurrent.ExecutionContext
import scala.concurrent.ExecutionContextExecutor
import de.htwb.wissrep.index.CosineSimilarity

object WebServer {
  def main(args: Array[String]) {

    implicit val system = ActorSystem("my-system")
    implicit val materializer = ActorMaterializer()
    // needed for the future flatMap/onComplete in the end
    implicit val executionContext = system.dispatcher

    val stream: InputStream = getClass.getResourceAsStream("view/index.html")
    val template = scala.io.Source.fromInputStream(stream)(Codec("ISO-8859-1")).mkString("")

    val searchRoute = IndexServer.getSearchRoute

    val searchURIs = 
      if(args.isEmpty)
        List("http://localhost:8080/search")
      else
        args.map("http://" + _ + "/search").toList

    val startPage =

      path("") {
        get {
          complete(HttpEntity(ContentTypes.`text/html(UTF-8)`, template.replace("{result}","")))
        }
      }

    val requestPage = path("") {
      formField("s") {
        search =>{
          val request = IndexRequest(search)
          val response = searchURIs.map(searchIndex(_, request)).reduce(_ + _)
          complete(HttpEntity(ContentTypes.`text/html(UTF-8)`,
            template.replace("{result}",{
              val cosSim = CosineSimilarity.sparse(response.results.map(_.tokens), response.completeDocumentCount)
              val req = Controller.transform(search)
              response.results
                .toSeq
                .sortBy(res => cosSim.getSimilarity(res.tokens, req))
                .map(doc => "<a href='" + doc.uri + "'  class='collection-item' target='_blank'> " + doc.title + "</a>")
                .mkString("")
              })
          )
          )
        }
      }
    }

    val route =
      if(args.isEmpty)
        concat(startPage, requestPage, searchRoute)
      else
        concat(startPage, requestPage)

    val bindingFuture = Http().bindAndHandle(route, "localhost", 8080)

    println(s"Server online at http://localhost:8080/\nPress RETURN to stop...")
    StdIn.readLine() // let it run until user presses return
    bindingFuture
      .flatMap(_.unbind()) // trigger unbinding from the port
      .onComplete(_ => system.terminate()) // and shutdown when done
  }

  def searchIndex(uri: String, request: IndexRequest)
      (implicit as: ActorSystem, mat : Materializer, ec: ExecutionContextExecutor): IndexResponse = {
    val reqJSON = write(request)
    println("To " + uri + " Send:\t" + reqJSON)
    val entity = HttpEntity(ContentTypes.`application/json`, reqJSON)
    val httpResponse = Http(as).singleRequest(HttpRequest(method = HttpMethods.POST, uri = uri, entity = entity))
    val responseFuture = httpResponse.flatMap(x => Unmarshal(x.entity).to[String])
    val response = Await.result(responseFuture, 1000.millis)
    val resp = read[IndexResponse](response)
    println(resp.results.size + " Documents were returned: \n\t" + response.substring(256))
    resp
  }
}
