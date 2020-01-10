import java.io.InputStream

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.model._
import akka.http.scaladsl.server.Directives._
import akka.stream.ActorMaterializer

import scala.io.{Codec, StdIn}

object WebServer {
  def main(args: Array[String]) {

    implicit val system = ActorSystem("my-system")
    implicit val materializer = ActorMaterializer()
    // needed for the future flatMap/onComplete in the end
    implicit val executionContext = system.dispatcher

    val stream: InputStream = getClass.getResourceAsStream("view/index.html")
    val template = scala.io.Source.fromInputStream(stream)(Codec("ISO-8859-1")).mkString("")
    val c = new Controller()
    c.loadInverseIndex("inverseIndex.bin")


    val startPage =

      path("") {
        get {
          complete(HttpEntity(ContentTypes.`text/html(UTF-8)`, template.replace("{result}","")))
        }
      }

    val requestPage = path("") {
      formField("s") {
        search =>
          complete(HttpEntity(ContentTypes.`text/html(UTF-8)`,
            template.replace("{result}",
              c.search(search)
                .map(nr => "<a href='https://www.berlin.de/polizei/polizeimeldungen/suche.php?q=Nr.+" + nr + "'  class='collection-item' target='_blank'>Nr " + nr + "</a>").mkString(""))
          )
          )
      }
    }

    val route = concat(startPage, requestPage)

    val bindingFuture = Http().bindAndHandle(route, "localhost", 8080)

    println(s"Server online at http://localhost:8080/\nPress RETURN to stop...")
    StdIn.readLine() // let it run until user presses return
    bindingFuture
      .flatMap(_.unbind()) // trigger unbinding from the port
      .onComplete(_ => system.terminate()) // and shutdown when done
  }
}
