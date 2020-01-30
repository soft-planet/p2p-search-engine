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

object WebServer {
  def main(args: Array[String]) {

    implicit val system = ActorSystem("my-system")
    implicit val materializer = ActorMaterializer()
    // needed for the future flatMap/onComplete in the end
    implicit val executionContext = system.dispatcher

    val stream: InputStream = getClass.getResourceAsStream("view/index.html")
    val template = scala.io.Source.fromInputStream(stream)(Codec("ISO-8859-1")).mkString("")

    val searchRoute = IndexServer.getSearchRoute

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
          val response = searchIndex("http://localhost:8080/search", request)
          complete(HttpEntity(ContentTypes.`text/html(UTF-8)`,
            template.replace("{result}",
              response.results
                .map(url => "<a href='"+ url + "'  class='collection-item' target='_blank'> " + url + "</a>").mkString(""))
          )
          )
        }
      }
    }

    val route = concat(startPage, requestPage, searchRoute)

    val bindingFuture = Http().bindAndHandle(route, "localhost", 8080)

    println(s"Server online at http://localhost:8080/\nPress RETURN to stop...")
    StdIn.readLine() // let it run until user presses return
    bindingFuture
      .flatMap(_.unbind()) // trigger unbinding from the port
      .onComplete(_ => system.terminate()) // and shutdown when done
  }

  def searchIndex(uri: String, request: IndexRequest)
      (implicit as: ActorSystem, mat : Materializer, ec: ExecutionContextExecutor): IndexResponse = {
    val entity = HttpEntity(ContentTypes.`application/json`, write(request))
    val httpResponse = Http(as).singleRequest(HttpRequest(method = HttpMethods.POST, uri = uri, entity = entity))
    val responseFuture = httpResponse.flatMap(x => Unmarshal(x.entity).to[String])
    val response = Await.result(responseFuture, 1000.millis)
    read[IndexResponse](response)
  }
}
