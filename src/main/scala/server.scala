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
import java.sql.{Connection, DriverManager}

object server extends App {
  private case class Game(
                   title: String,
                   thumbnail: String,
                   short_description: String,
                   game_url: String,
                   genre: String,
                   platform: String,
                   publisher: String,
                   developer: String,
                   release_date: String,
                   freetogame_profile_url: String
                 )
   private object GameProtocol extends DefaultJsonProtocol {
    implicit val gameFormat: RootJsonFormat[Game] = jsonFormat10(Game)
  }

  implicit val actorSystem: ActorSystem = ActorSystem()
  implicit val ec: ExecutionContextExecutor = actorSystem.dispatcher

  implicit class MapToJson[V](params: Map[String, V]) {
    def toUrlParams: String = params.map { case (k, v) => s"$k=$v" }.mkString("&")
  }

  private val route = {
    import GameProtocol._
    concat(
      path("api") {
        val connection: Connection = DriverManager.getConnection("jdbc:sqlite:db.sqlite")
        get {
          connection.createStatement().execute("CREATE TABLE IF NOT EXISTS apiResponse (id SERIAL PRIMARY KEY, jsonResponse TEXT, queryParams TEXT);")

          extractUri { uri =>
            val platform = uri.query().get("platform").getOrElse("")
            val category = uri.query().get("category").getOrElse("")
            val currentParameters = s"platform=$platform&category=$category"

            parameters("platform", Symbol("category")) { (platform, category) =>
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

              try {
                var dbResponse = ""
                var dbParameters = ""

                val statementSelect = connection.prepareStatement("SELECT * FROM apiResponse WHERE queryParams = ?")
                statementSelect.setString(1, currentParameters)
                val result = statementSelect.executeQuery()
                while (result.next()) {
                  dbResponse = result.getString(2)
                  dbParameters = result.getString(3)
                }

                if (currentParameters == dbParameters) {
                  dbResponse.parseJson.convertTo[List[Game]].toJson
                  complete(HttpEntity(ContentTypes.`application/json`, s"$dbResponse"))
                } else {
                  val responseFuture = Http().singleRequest(freeTopGameRequest)
                  val awaitResponse = Await.result(responseFuture.flatMap(resp => Unmarshal(resp.entity).to[String]), 10.seconds)
                  awaitResponse.parseJson.convertTo[List[Game]].toJson

                  val statement = connection.prepareStatement("INSERT INTO apiResponse values ((SELECT COALESCE(max(id)+1, 1) from apiResponse), ?, ?)")
                  statement.setString(1, awaitResponse)
                  statement.setString(2, currentParameters)
                  statement.executeUpdate()

                  responseFuture.value match {
                    case Some(Success(_)) => complete(HttpEntity(ContentTypes.`application/json`, s"$awaitResponse"))
                    case Some(Failure(e)) => complete(InternalServerError, s"An error occurred while processing your request: $e")
                  }
                }
              } finally {
                connection.close()
              }
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