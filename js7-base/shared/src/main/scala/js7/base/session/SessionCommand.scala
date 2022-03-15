package js7.base.session

import io.circe.generic.semiauto
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, Json, JsonObject}
import js7.base.auth.{SessionToken, UserAndPassword}
import js7.base.circeutils.CirceCodec
import js7.base.circeutils.CirceUtils.singletonCodec
import js7.base.circeutils.typed.{Subtype, TypedJsonCodec}
import js7.base.generic.SecretString
import js7.base.version.Version

/**
  * @author Joacim Zschimmer
  */
sealed trait SessionCommand {
  type Response
}

object SessionCommand
{
  /** Authenticate a user and establish a session.
    * @param userAndPassword if None, the HTTP header authentication is used to allow browser authentication dialog.
    */
  final case class Login(
    userAndPassword: Option[UserAndPassword],
    js7Version: Option[Version])
  extends SessionCommand {
    type Response = Login.LoggedIn
  }

  object Login {
    implicit val x = UserAndPassword.jsonCodec
    implicit val jsonEncoder: Encoder.AsObject[Login] =
      o => JsonObject(
        "userAndPassword" -> o.userAndPassword.asJson,
        "version" -> o.js7Version.asJson,  // COMPATIBLE with v2.2
        "js7Version" -> o.js7Version.asJson)
    implicit val jsonDecoder = semiauto.deriveDecoder[Login]

    final case class LoggedIn(sessionToken: SessionToken, js7Version: Version)
    extends SessionCommand.Response

    object LoggedIn {
      implicit val jsonEncoder: Encoder.AsObject[LoggedIn] =
        o => JsonObject(
          "sessionToken" -> o.sessionToken.secret.string.asJson,
          "js7Version" -> o.js7Version.asJson)

      implicit val jsonDecoder: Decoder[LoggedIn] =
        cursor =>
          for {
            token <- cursor.get[String]("sessionToken")
            version <- cursor.get[Version]("js7Version")
          } yield LoggedIn(SessionToken(SecretString(token)), js7Version = version)
    }
  }

  /** Invalidate the session established by `Login`.
    */
  final case class Logout(sessionToken: SessionToken) extends SessionCommand {
    type Response = SessionCommand.Response.Accepted
  }
  object Logout {
    implicit val jsonEncoder: Encoder.AsObject[Logout] =
      o => JsonObject("sessionToken" -> Json.fromString(o.sessionToken.secret.string))

    implicit val jsonDecoder: Decoder[Logout] =
      cursor =>
        for (token <- cursor.get[String]("sessionToken")) yield
          Logout(SessionToken(SecretString(token)))
  }

  sealed trait Response
  object Response {
    sealed trait Accepted extends Response
    case object Accepted extends Accepted {
      implicit val jsonCodec: CirceCodec[Accepted.type] = singletonCodec(Accepted)
    }

    implicit val jsonCodec = TypedJsonCodec[Response](
      Subtype(Response.Accepted),
      Subtype[Login.LoggedIn])
  }

  implicit val jsonCodec = {
    implicit val x = UserAndPassword.jsonCodec
    TypedJsonCodec[SessionCommand](
      Subtype[Login],
      Subtype[Logout])
  }
}
