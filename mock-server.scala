//> using scala "3.3.3"
//> using dep "org.http4s::http4s-ember-server:0.23.26"
//> using dep "org.http4s::http4s-dsl:0.23.26"
//> using dep "org.http4s::http4s-circe:0.23.26"
//> using dep "io.circe::circe-generic:0.14.6"

import cats.effect.*
import com.comcast.ip4s.*
import io.circe.generic.auto.*
import org.http4s.*
import org.http4s.circe.*
import org.http4s.dsl.io.*
import org.http4s.ember.server.*
import org.http4s.implicits.*
import scala.util.Random

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

object MockBankIdServer extends IOApp.Simple {

  // This implicit provides the magic to decode a JSON body into our SigninRequest case class.
  implicit val signinRequestDecoder: EntityDecoder[IO, SigninRequest] = jsonOf[IO, SigninRequest]

  // Define the single endpoint for our mock server.
  val bankIdRoutes: HttpRoutes[IO] = HttpRoutes.of[IO] {

    case req @ POST -> Root / "signin" =>
      for {
        // Decode the JSON body of the request into our case class.
        // If decoding fails, http4s will automatically generate a 400 Bad Request response.
        signinRequest <- req.as[SigninRequest]

        // --- Logging incoming request details ---
        _ <- IO.println("--- Received a new request! ---")
        _ <- IO.println(s"Client IP: ${req.remoteAddr.map(_.toString).getOrElse("N/A")}")
        _ <- IO.println(s"Method: ${req.method}")
        _ <- IO.println(s"URI: ${req.uri}")
        _ <- IO.println(s"Headers: \n${req.headers.headers.mkString("  ", "\n  ", "")}")
        _ <- IO.println(s"Cookies: ${if (req.cookies.isEmpty) "None" else req.cookies.mkString(", ")}")
        _ <- IO.println(s"Decoded Body: \n$signinRequest")
        _ <- IO.println("-------------------------------------------------")

        // --- Handle response ---
        // Generate a random integer for the session cookie.
        cookieValue = Random.nextInt(Int.MaxValue)

        // Log the cookie value so we can see it on the server side.
        _ <- IO.println("\n***************")
        _ <- IO.println(s"Setting response cookie 'session-cookie' with value: $cookieValue")
        _ <- IO.println("***************\n")

        // Create the cookie.
        sessionCookie = ResponseCookie(name = "session-cookie", content = cookieValue.toString)

        // Create a 200 OK response and attach the cookie to it.
        response <- Ok().map(_.addCookie(sessionCookie))
      } yield response
  }

  // The main entry point to build and run the server.
  def run: IO[Unit] =
    EmberServerBuilder
      .default[IO]
      .withHost(ipv4"0.0.0.0")
      .withPort(port"8080")
      .withHttpApp(bankIdRoutes.orNotFound)
      .build
      .use { server =>
        IO.println(s"🚀 Server started at ${server.address}. Waiting for POST requests to /signin...") *>
        IO.never
      }
      .as(ExitCode.Success)
}
