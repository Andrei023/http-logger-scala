//> using scala "3.3.3"
//> using dep "org.http4s::http4s-ember-server:0.23.26"
//> using dep "org.http4s::http4s-dsl:0.23.26"
//> using dep "org.http4s::http4s-circe:0.23.26"
//> using dep "io.circe::circe-generic:0.14.6"

import cats.effect.*
import com.comcast.ip4s.*
import io.circe.*
import io.circe.generic.auto.*
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

case class WrappedSignin(data: SigninRequest)

// --- Flexible decoder: accepts either flat or { "data": { ... } } ---
object FlexibleDecoders {
  implicit val signinRequestDecoderFlexible: Decoder[SigninRequest] =
    Decoder.instance { c =>
      // Try wrapped first: {"data": { ... }}
      c.downField("data").as[SigninRequest].orElse {
        // Fallback to flat object
        c.as[SigninRequest]
      }
    }

  // http4s EntityDecoder using the flexible Circe decoder
  implicit val entityDecoderSignin: EntityDecoder[IO, SigninRequest] =
    jsonOf[IO, SigninRequest](implicitly, signinRequestDecoderFlexible)
}

object MockBankIdServer extends IOApp.Simple {
  import FlexibleDecoders.*

  val routes: HttpRoutes[IO] = HttpRoutes.of[IO] {
    case req @ POST -> Root / "signin" =>
      for {
        signinRequest <- req.as[SigninRequest]
        _ <- IO.println("--- Received a new request! ---")
        _ <- IO.println(s"Client IP: ${req.remoteAddr.map(_.toString).getOrElse("N/A")}")
        _ <- IO.println(s"Method: ${req.method}")
        _ <- IO.println(s"URI: ${req.uri}")
        _ <- IO.println(s"Headers: \n${req.headers.headers.mkString("  ", "\n  ", "")}")
        _ <- IO.println(s"Decoded Body (SigninRequest): \n$signinRequest")
        _ <- IO.println("-------------------------------------------------")

        // Set a cookie like before
        cookieValue = Random.nextInt(Int.MaxValue)
        sessionCookie = ResponseCookie(name = "session-cookie", content = cookieValue.toString)

        res <- Ok().map(_.addCookie(sessionCookie))
      } yield res
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
        IO.println(s"🚀 Server started at ${server.address}. POST /signin") *> IO.never
      }
      .as(ExitCode.Success)
  }
}
