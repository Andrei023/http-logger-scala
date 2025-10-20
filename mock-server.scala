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
import org.http4s.*
import org.http4s.circe.*
import org.http4s.dsl.io.*
import org.http4s.ember.server.*
import org.http4s.implicits.*
import scala.util.Random

// --- Domain ---
case class SigninRequest(
  txRefId: String,
  name: String,
  firstName: String,
  lastName: String,
  personid: String,
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
  merchantId: Option[Int],
)

// --- Flexible decoder to support flat or wrapped "data" format ---
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

  val routes: HttpRoutes[IO] = HttpRoutes.of[IO] {

    case req @ POST -> Root / "signin" =>
      // attempt decode + log failures manually
      req
        .attemptAs[SigninRequest]
        .value
        .flatMap {
          case Right(signinRequest) =>
            for {
              _ <- IO.println("--- ✅ Received new request ---")
              _ <- IO.println(s"Client IP: ${req.remoteAddr.map(_.toString).getOrElse("N/A")}")
              _ <- IO.println(s"Method: ${req.method}")
              _ <- IO.println(s"URI: ${req.uri}")
              _ <- IO.println(s"Headers: \n${req.headers.headers.mkString("  ", "\n  ", "")}")
              _ <- IO.println(s"Decoded Body: $signinRequest")
              _ <- IO.println("-------------------------------------------------\n")

              cookieValue = Random.nextInt(Int.MaxValue)
              sessionCookie = ResponseCookie(name = "session-cookie", content = cookieValue.toString)

              resp <- Ok().map(_.addCookie(sessionCookie))
            } yield resp

          case Left(err) =>
            // Circe decoding failure
            for {
              _ <- IO.println("❌ JSON decoding failed:")
              _ <- IO.println(err)
              body <- req.as[String].attempt.map {
                case Right(raw) => raw
                case Left(_)    => "<failed to read raw body>"
              }
              _ <- IO.println(s"Raw body:\n$body")
              _ <- IO.println("-------------------------------------------------\n")
              resp <- BadRequest(Json.obj(
                "error" -> Json.fromString("Invalid JSON or structure"),
                "details" -> Json.fromString(err.getMessage)
              ))
            } yield resp
        }
        .handleErrorWith { ex =>
          // catch-all for any other runtime errors
          for {
            _ <- IO.println(s"💥 Internal error: ${ex.getMessage}")
            _ <- IO.println(ex)
            _ <- IO.println("-------------------------------------------------\n")
            resp <- InternalServerError(Json.obj(
              "error" -> Json.fromString("Internal Server Error"),
              "details" -> Json.fromString(ex.toString)
            ))
          } yield resp
        }
  }

  def run: IO[Unit] = {
    val portNum = sys.env.get("PORT").flatMap(_.toIntOption).getOrElse(8080)
    val port    = Port.fromInt(portNum).getOrElse(port"8080")

    EmberServerBuilder
      .default[IO]
      .withHost(ipv4"0.0.0.0")
      .withPort(port)
      .withHttpApp(routes.orNotFound)
      .build
      .use { server =>
        IO.println(s"🚀 Server running at ${server.address}. POST /signin") *>
        IO.never
      }
      .as(ExitCode.Success)
  }
}
