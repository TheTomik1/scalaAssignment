import scala.io.StdIn.readLine
import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.{HttpEntity, HttpMethod, HttpMethods, HttpRequest, RequestEntity}
import akka.http.scaladsl.unmarshalling.Unmarshal
import scala.concurrent.{ExecutionContextExecutor, Future}

object client extends App {
  implicit val as: ActorSystem = ActorSystem()
  implicit val ec: ExecutionContextExecutor = as.dispatcher
  case class Response(status: Int, body: String)

  print("Choose platform (pc, browser): ")
  private val platform = readLine()

  print("Choose category (shooter, mmorpg): ")
  private val category = readLine()
  def request(method: HttpMethod, path: String = "", params: Option[String] = None, body: Option[RequestEntity] = None): HttpRequest =
    HttpRequest(
      method = method,
      uri = s"http://localhost:8080$path${params.map(p => s"?$p").getOrElse("")}",
      headers = Seq(),
      entity = body.getOrElse(HttpEntity.Empty)
    )
  implicit class UltraHttpRequest(request: HttpRequest) {
    def exec: Future[Response] =
      for {
        response <- Http().singleRequest(request)
        status = response.status.intValue()
        body <- Unmarshal(response.entity).to[String]
        _ = response.entity.discardBytes()
      } yield Response(status, body)
  }
  private val runF = for {
    _ <- request(HttpMethods.GET, path = "/api", params = Some(s"platform=$platform&category=$category")).exec.map(println)
  } yield ()
  runF.andThen(_ => as.terminate())
}