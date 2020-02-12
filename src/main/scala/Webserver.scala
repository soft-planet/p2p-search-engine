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
import de.htwb.wissrep.index.CosineSimilarity._
import scala.concurrent.Future

object WebServer {
  def main(args: Array[String]) {

    implicit val system = ActorSystem("my-system")
    implicit val materializer = ActorMaterializer()
    // needed for the future flatMap/onComplete in the end
    implicit val executionContext = system.dispatcher

    val stream: InputStream = getClass.getResourceAsStream("view/index.html")
    val template = scala.io.Source.fromInputStream(stream)(Codec("ISO-8859-1")).mkString("")

    val searchRoute = IndexServer.getSearchRoute

    val allIps = scala.io.Source.fromFile("webserver_config.txt").getLines().toList

    val searchURIs = 
      if(!args.isEmpty)
        List("http://localhost:8080/search")
      else
        allIps.map("http://" + _ + "/search")


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
          val response = searchIndices(searchURIs, request).reduce(_ + _)
          complete(HttpEntity(ContentTypes.`text/html(UTF-8)`,
            template.replace("{result}",{
              val cosSim = CosineSimilarity.sparse(response.results, response.completeDocumentCount)
              val req = Controller.transform(search)
              response.results
                .toSeq
                .distinct
                .sortBy(res => -cosSim.getSimilarity(res.tokenCounts, req))
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

    val accessPoint = scala.io.Source.fromFile("accesspoint.txt").getLines().toList

    val bindingFuture = Http().bindAndHandle(route, accessPoint.head, 8080)

    println(s"Server online at http://" + accessPoint.head +":8080/\nPress RETURN to stop...")
    StdIn.readLine() // let it run until user presses return
    bindingFuture
      .flatMap(_.unbind()) // trigger unbinding from the port
      .onComplete(_ => system.terminate()) // and shutdown when done
  }

  def searchIndices(uris: Iterable[String], request: IndexRequest)
      (implicit as: ActorSystem, mat : Materializer, ec: ExecutionContextExecutor): List[IndexResponse] = {
    uris.toList.map(uri => searchIndex(uri, request)).map(Await.result(_, 10.seconds))
  }

  def searchIndex(uri: String, request: IndexRequest)
      (implicit as: ActorSystem, mat : Materializer, ec: ExecutionContextExecutor): Future[IndexResponse] = {
    val reqJSON = write(request)
    println("To " + uri + " Send:\t" + reqJSON)
    val entity = HttpEntity(ContentTypes.`application/json`, reqJSON)
    val httpResponse = Http(as).singleRequest(HttpRequest(method = HttpMethods.POST, uri = uri, entity = entity))
    val responseFuture = httpResponse
                            .flatMap(x => Unmarshal(x.entity).to[String])
                            .map(read[IndexResponse](_))
                            .map(resp => {println(resp.results.size + " Documents were returned"); resp}) // print the Responses
    responseFuture
  }

}
