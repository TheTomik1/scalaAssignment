import scala.io.StdIn.readLine

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.headers.RawHeader
import akka.http.scaladsl.model.{HttpMethods, HttpRequest}
import akka.http.scaladsl.unmarshalling.Unmarshal

import scala.concurrent.ExecutionContextExecutor
object restApi extends App {
  implicit val as: ActorSystem = ActorSystem()
  implicit val ec: ExecutionContextExecutor = as.dispatcher
  implicit class MapToJson[V](params: Map[String, V]) {
    def toUrlParams: String = params.map { case (k, v) => s"$k=$v" }.mkString("&")
  }

  print("Choose platform (pc, browser): ")
  private val platform = readLine()

  print("Choose category (shooter, mmorpg): ")
  private val category = readLine()

  private val requestParams = Map(
    "platform" -> platform,
    "category" -> category
  ).toUrlParams

  private val request = HttpRequest(
    method = HttpMethods.GET,
    uri = s"https://www.freetogame.com/api/games?$requestParams",
    headers = Seq(
      RawHeader("Accept", "application/json")
    ),
  )

  private val performRequestFut = for {
    response <- Http().singleRequest(request)
    body <- Unmarshal(response.entity).to[String]
    _ = response.entity.discardBytes()
  } yield println(body)
  performRequestFut.andThen(_ => as.terminate()) // we do not need Await.result because of as running
}