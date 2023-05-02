import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.StatusCodes.InternalServerError
import akka.http.scaladsl.model._
import akka.http.scaladsl.model.headers.RawHeader
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.unmarshalling.Unmarshal
import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, ExecutionContextExecutor}
import scala.io.StdIn
import scala.util.{Failure, Success}
import spray.json._
import DefaultJsonProtocol._

object server extends App {
    implicit val actorSystem: ActorSystem = ActorSystem()
    implicit val ec: ExecutionContextExecutor = actorSystem.dispatcher

    implicit class MapToJson[V](params: Map[String, V]) {
      def toUrlParams: String = params.map { case (k, v) => s"$k=$v" }.mkString("&")
    }

    private val route = {
      concat(
        path("api") {
          get {
            parameters("platform", Symbol("category")) { (platform, category)  =>
              val requestParams = Map(
                "platform" -> platform,
                "category" -> category
              ).toUrlParams

              val freeTopGameRequest = HttpRequest(
                method = HttpMethods.GET,
                uri = s"https://www.freetogame.com/api/games?$requestParams",
                headers = Seq(
                  RawHeader("Accept", "application/json")
                )
              )

              val responseFuture = Http().singleRequest(freeTopGameRequest)
              val awaitResponse = Await.result(responseFuture.flatMap(resp => Unmarshal(resp.entity).to[String]), 10.seconds)
              val jsonResponse = awaitResponse.parseJson

              responseFuture.value match {
                case Some(Success(_)) => complete(HttpEntity(ContentTypes.`application/json`, s"$jsonResponse"))
                case Some(Failure(e)) => complete(InternalServerError, s"An error occurred while processing your request: $e")
              }
            }
          }
        },
        pathEndOrSingleSlash {
          get {
            complete(HttpEntity(ContentTypes.`text/html(UTF-8)`, "api path not given!"))
          }
        }
      )
    }

    private val bindingFut = for {
      binding <- Http().newServerAt("localhost", 8080).bind(route)
      _ = println(s"Server running on ${binding.localAddress.getHostName}:${binding.localAddress.getPort}")
    } yield binding
    StdIn.readLine()
    bindingFut.flatMap(_.unbind()).andThen(_ => actorSystem.terminate())
  }

