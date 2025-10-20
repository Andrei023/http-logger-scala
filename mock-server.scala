//> using scala "3.3.3"
//> using dep "org.http4s::http4s-ember-server:0.23.26"
//> using dep "org.http4s::http4s-dsl:0.23.26"
//> using dep "org.http4s::http4s-circe:0.23.26"
//> using dep "io.circe::circe-generic:0.14.6"
//> using dep "io.circe::circe-parser:0.14.6"

import cats.effect.*
import com.comcast.ip4s.*
import io.circe.*
import io.circe.generic.semiauto.*   // use semiauto deriveDecoder
import org.http4s.*
import org.http4s.circe.*
import org.http4s.dsl.io.*
import org.http4s.ember.server.*
import org.http4s.implicits.*
import org.http4s.server.middleware.{ Logger => Http4sLogger }
import scala.util.Random

// ---- Domain (everything optional so minimal payloads decode) ----
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

// ---- Accept either flat or wrapped {"data": {...}} safely (no recursion) ----
object FlexibleDecoders {
  // Base decoder only knows flat SigninRequest (not implicit to avoid ambiguity)
  private val signinBaseDecoder: Decoder[SigninRequest] = deriveDecoder[SigninRequest]

  // Flexible decoder: try wrapped first, then flat — both use the base decoder
  implicit val signinRequestDecoderFlexible: Decoder[SigninRequest] =
    Decoder.instance { c =>
      c.downField("data").as(signinBaseDecoder).orElse(signinBaseDecoder(c))
    }

  // http4s entity decoder using the flexible Circe decoder (passed explicitly)
  implicit val entityDecoderSignin: EntityDecoder[IO, SigninRequest] =
    jsonOf[IO, SigninRequest](implicitly, signinRequestDecoderFlexible)
}

object MockBankIdServer extends IOApp.Simple {
  import FlexibleDecoders.*

  // ---------- Routes ----------
  val routes: HttpRoutes[IO] = HttpRoutes.of[IO] {

    // Health/version (confirm deploys)
    case GET -> Root / "__health"  => Ok("ok")
    case GET -> Root / "__version" => Ok(BuildInfoTxt)

    // RAW endpoint: logs exactly what arrived; NEVER 500s
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
        res   <- Ok(io.circe.Json.obj("ok" -> Json.True)).map(_.addCookie(cookie))
      } yield res

    // Decode into case class, 400 on bad JSON, 200 on success
    case req @ POST -> Root / "signin" =>
      req.attemptAs[SigninRequest].value.flatMap {
        case Right(in) =>
          for {
            _ <- IO.println("\n--- ✅ /signin decoded ---")
            _ <- IO.println(s"Headers:\n${req.headers.headers.mkString("  ", "\n  ", "")}")
            _ <- IO.println(s"Decoded:\n$in")
            _ <- IO.println("--------------------------------\n")
            cookie = ResponseCookie("session-cookie", Random.nextInt(Int.MaxValue).toString)
            res <- Ok(
              Json.obj(
                "ok"        -> Json.True,
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
                "details" -> Json.fromString(Option(err.getMessage).getOrElse(err.toString))
              )
            )
          } yield res
      }
  }

  // ---------- App with robust error logging ----------
  def run: IO[Unit] = {
    val port = sys.env.get("PORT").flatMap(_.toIntOption).flatMap(Port.fromInt).getOrElse(port"8080")

    val baseApp: HttpApp[IO] = routes.orNotFound

    // Guarded app: catch ANY unhandled exception and return JSON 500 (with logs)
    val guarded: HttpApp[IO] = HttpApp[IO] { req =>
      baseApp(req).handleErrorWith { ex =>
        IO.println("\n💥 TOP-LEVEL ERROR (unhandled):") *>
        IO.println(ex) *>
        IO.println("--------------------------------\n") *>
        InternalServerError(
          Json.obj(
            "error"   -> Json.fromString("Internal Server Error"),
            "details" -> Json.fromString(Option(ex.getMessage).getOrElse(ex.toString))
          )
        )
      }
    }

    // Add request/response logging (headers + bodies)
    val loggedApp: HttpApp[IO] =
      Http4sLogger.httpApp(logHeaders = true, logBody = true)(guarded)

    EmberServerBuilder
      .default[IO]
      .withHost(ipv4"0.0.0.0")
      .withPort(port)
      .withHttpApp(loggedApp)
      .build
      .use { srv =>
        IO.println(s"🚀 Running at ${srv.address}  |  POST /signin  &  /signin-raw  |  GET /__health /__version") *>
        IO.never
      }
      .as(ExitCode.Success)
  }

  // Simple version string so each deploy shows a new value
  private val BuildInfoTxt =
    s"version=debug-logging-OK-2  time=${java.time.Instant.now}"
}
