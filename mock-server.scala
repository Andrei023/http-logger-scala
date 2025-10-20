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

// Make every field optional so a minimal JSON from Spring decodes
// Missing fields will be decoded as None.
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

  private def okJson(obj: Json): IO[Response[IO]] =
    Ok(obj).map(_.putHeaders(Header.Raw(ci"Content-Type", "application/json")))

  val routes: HttpRoutes[IO] = HttpRoutes.of[IO] {
    case req @ POST -> Root / "signin" =>
      // Decode with logs; never explode — return 400 on bad JSON
      req.attemptAs[SigninRequest].value.flatMap {
        case Right(in) =>
          for {
            _ <- IO.println("--- ✅ Received /signin ---")
            _ <- IO.println(s"Headers:\n${req.headers.headers.mkString("  ", "\n  ", "")}")
            _ <- IO.println(s"Decoded:\n$in")
            cookie = ResponseCookie("session-cookie", Random.nextInt(Int.MaxValue).toString)
            resp   <- okJson(Json.obj(
                        "ok" -> Json.True,
                        "txRefId" -> Json.fromString(in.txRefId.getOrElse("")),
                        "sessionId" -> Json.fromString(in.sessionId.getOrElse(""))
                      )).map(_.addCookie(cookie))
          } yield resp

        case Left(err) =>
          for {
            raw <- req.as[String].attempt.map(_.getOrElse("<failed to read body>"))
            _   <- IO.println("❌ JSON decode error on /signin")
            _   <- IO.println(err)
            _   <- IO.println(s"Raw body:\n$raw")
            resp <- BadRequest(Json.obj(
                      "error" -> Json.fromString("Invalid JSON"),
                      "details" -> Json.fromString(err.getMessage)
                    ))
          } yield resp
      }.handleErrorWith { ex =>
        for {
          _ <- IO.println(s"💥 Internal error: ${ex.getMessage}")
          _ <- IO.println(ex)
          resp <- InternalServerError(Json.obj(
                    "error" -> Json.fromString("Internal Server Error"),
                    "details" -> Json.fromString(ex.toString)
                  ))
        } yield resp
      }
  }

  def run: IO[Unit] = {
    val port = sys.env.get("PORT").flatMap(_.toIntOption).flatMap(Port.fromInt).getOrElse(port"8080")
    EmberServerBuilder
      .default[IO]
      .withHost(ipv4"0.0.0.0")
      .withPort(port)
      .withHttpApp(routes.orNotFound)
      .build
      .use { srv =>
        IO.println(s"🚀 POST /signin on ${srv.address}") *> IO.never
      }
      .as(ExitCode.Success)
  }
}
