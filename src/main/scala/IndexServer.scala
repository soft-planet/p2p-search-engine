import java.io.FileInputStream

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.model._
import akka.http.scaladsl.server.Directives._
import akka.stream.ActorMaterializer
import akka.http.scaladsl.server.Route

import scala.io.StdIn
import scala.concurrent.Await
import scala.concurrent.duration._
import upickle.default._
import upickle.default.{macroRW, ReadWriter => RW}
import ujson.{Arr, Obj, Value}


final case class IndexRequest(request: String)
object IndexRequest{
  implicit val rw: RW[IndexRequest] = macroRW
}
final case class IndexResponse(results: Map[String, String])
object IndexResponse{
  implicit val rw: RW[IndexResponse] = macroRW
}

final case class Peer(ip:String, port:Int)
object Peer{
  implicit val rw: RW[Peer] = macroRW
}

object IndexServer {

  def main(args: Array[String]) {

    implicit val system = ActorSystem("my-system")

    implicit val materializer = ActorMaterializer()
    // needed for the future flatMap/onComplete in the end
    implicit val executionContext = system.dispatcher

    val input = new FileInputStream("peer_config.json")

    val content = read[Peer](input)

    val bindingFuture = Http().bindAndHandle(getSearchRoute, content.ip , content.port) //"localhost", 8081)

    println(s"Server online at http://" + content.ip + ":" + content.port + "/\nPress RETURN to stop...")
    StdIn.readLine() // let it run until user presses return
    bindingFuture
      .flatMap(_.unbind()) // trigger unbinding from the port
      .onComplete(_ => system.terminate()) // and shutdown when done
  }

  def getSearchRoute: Route = {
    val controller = new Controller()
    controller.loadInverseIndex("inverseIndex.bin")
    val route =
      path("search") {
        post {
          decodeRequest{
            entity(as[String]){ jsonRequest => {
                val request = read[IndexRequest](jsonRequest)
                val requestString = request.request
                val responseList = controller.search(requestString)
                val response = IndexResponse(responseList)
                val jsonResponse = write(response)
                complete(HttpEntity(ContentTypes.`application/json`, jsonResponse))
              }
            }
          }
        }
      }
      route
    }
}