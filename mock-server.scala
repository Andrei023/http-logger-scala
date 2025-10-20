//> using scala "3.3.3"
//> using dep "org.http4s::http4s-ember-server:0.23.26"
//> using dep "org.http4s::http4s-dsl:0.23.26"
//> using dep "org.http4s::http4s-circe:0.23.26"
//> using dep "io.circe::circe-generic:0.14.6"

import cats.effect.*
import com.comcast.ip4s.*
import io.circe.*
import io.circe.generic.auto.*
import io.circe.syntax.*
import io.circe.parser.*
import org.http4s.*
import org.http4s.circe.*
import org.http4s.dsl.io.*
import org.http4s.ember.server.*
import org.http4s.implicits.*
import org.http4s.server.middleware.{ Logger => Http4sLogger }
import scala.util.Random

// ---- domain (make everything optional so minimal payload decodes) ----
case class SigninRequest(
  txRefId: Option[String],
  name: Option[String],
  firstName: Option[String],
  lastName: Option[String],
  personid: Option[String],
  gender: Option[String],
  street: Option[String],
  city: Option[String],
  zip: Option[String],
  country: Option[String],
  dob: Option[String],
  ip: Option[String],
  attributes: Option[Map[String, String]],
  kycentityid: Option[String],
  sessionId: Option[String],
  identityProvider: Option[String],
  merchantId: Option[Int]
)

// Accept either flat or wrapped {"data": {...}}
object FlexibleDecoders {
  implicit val signinRequestDecoderFlexible: Decoder[SigninRequest] =
    Decoder.instance { c =>
      c.downField("data").as[SigninRequest].orElse(c.as[SigninRequest])
    }
  implicit val entityDecoderSignin: EntityDecoder[IO, SigninRequest] =
    jsonOf[IO, SigninRequest](implicitly, signinRequestDecoderFlexible)
}

object MockBankIdServer extends IOApp.Simple {
  import FlexibleDecoders.*

  private def jsonOk(j: Json): IO[Response[IO]] =
    Ok(j).map(_.putHeaders(Header.Raw(ci"Content-Type", "application/json")))

  // ---------- routes ----------
  val routes: HttpRoutes[IO] = HttpRoutes.of[IO] {

    // 0) health/version to confirm deployment
    case GET -> Root / "__health"  => Ok("ok")
    case GET -> Root / "__version" => Ok(BuildInfoTxt) // define below or hardcode a string

    // 1) RAW endpoint: logs exactly what arrived; NEVER 500s
    case req @ POST -> Root / "signin-raw" =>
      for {
        body <- req.as[String].attempt
        _    <- IO.println("\n--- 📥 /signin-raw incoming ---")
        _    <- IO.println(s"Method: ${req.method}  URI: ${req.uri}")
        _    <- IO.println(s"Headers:\n${req.headers.headers.mkString("  ", "\n  ", "")}")
        _    <- IO.println(s"Body parse status: ${body.isRight}")
        _    <- IO.println(body.fold(e => s"Body ERROR: ${e.getMessage}", b => s"Body:\n$b"))
        _    <- IO.println("--------------------------------\n")
        cookie = ResponseCookie("session-cookie", Random.nextInt(Int.MaxValue).toString)
        res   <- jsonOk(Json.obj("ok" -> Json.True)).map(_.addCookie(cookie))
      } yield res

    // 2) Strict-ish endpoint: decode into case class, 400 on bad JSON, 200 on success
    case req @ POST -> Root / "signin" =>
      req.attemptAs[SigninRequest].value.flatMap {
        case Right(in) =>
          for {
            _ <- IO.println("\n--- ✅ /signin decoded ---")
            _ <- IO.println(s"Headers:\n${req.headers.headers.mkString("  ", "\n  ", "")}")
            _ <- IO.println(s"Decoded:\n$in")
            _ <- IO.println("--------------------------------\n")
            cookie = ResponseCookie("session-cookie", Random.nextInt(Int.MaxValue).toString)
            res <- jsonOk(
              Json.obj(
                "ok" -> Json.True,
                "txRefId"   -> Json.fromString(in.txRefId.getOrElse("")),
                "sessionId" -> Json.fromString(in.sessionId.getOrElse(""))
              )
            ).map(_.addCookie(cookie))
          } yield res

        case Left(err) =>
          for {
            raw <- req.as[String].attempt.map(_.getOrElse("<failed to read body>"))
            _   <- IO.println("\n❌ JSON decode error on /signin")
            _   <- IO.println(err)
            _   <- IO.println(s"Raw body:\n$raw")
            _   <- IO.println("--------------------------------\n")
            res <- BadRequest(
              Json.obj(
                "error"   -> Json.fromString("Invalid JSON"),
                "details" -> Json.fromString(err.getMessage)
              )
            )
          } yield res
      }
  }

  // ---------- app with robust error logging ----------
  def run: IO[Unit] = {
    val port = sys.env.get("PORT").flatMap(_.toIntOption).flatMap(Port.fromInt).getOrElse(port"8080")

    val baseApp: HttpApp[IO] = routes.orNotFound

    // Wrap with:
    //  - http4s built-in request/response logger (headers + bodies)
    //  - a top-level .handleErrorWith that logs + returns JSON 500
    val loggedApp: HttpApp[IO] =
      Http4sLogger.httpApp(logHeaders = true, logBody = true)(
        baseApp.handleErrorWith { ex =>
          for {
            _ <- IO.println("\n💥 TOP-LEVEL ERROR (unhandled):")
            _ <- IO.println(ex)
            _ <- IO.println("--------------------------------\n")
            res <- InternalServerError(
              Json.obj(
                "error"   -> Json.fromString("Internal Server Error"),
                "details" -> Json.fromString(Option(ex.getMessage).getOrElse(ex.toString))
              )
            )
          } yield res
        }
      )

    EmberServerBuilder
      .default[IO]
      .withHost(ipv4"0.0.0.0")
      .withPort(port)
      .withHttpApp(loggedApp)
      .build
      .use { srv =>
        IO.println(s"🚀 Running at ${srv.address}  |  POST /signin  &  /signin-raw") *> IO.never
      }
      .as(ExitCode.Success)
  }

  // Tiny version string; replace with your build info if you like
  private val BuildInfoTxt =
    s"version=debug-logging-1  time=${java.time.Instant.now}"
}
