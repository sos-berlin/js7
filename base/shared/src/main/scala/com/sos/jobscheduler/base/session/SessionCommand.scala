package com.sos.jobscheduler.base.session

import com.sos.jobscheduler.base.auth.{SessionToken, UserAndPassword}
import com.sos.jobscheduler.base.circeutils.CirceCodec
import com.sos.jobscheduler.base.circeutils.CirceUtils.{deriveCodec, singletonCodec}
import com.sos.jobscheduler.base.circeutils.typed.{Subtype, TypedJsonCodec}
import com.sos.jobscheduler.base.generic.SecretString
import io.circe.{Decoder, Json, JsonObject, ObjectEncoder}

/**
  * @author Joacim Zschimmer
  */
sealed trait SessionCommand {
  type Response
}

object SessionCommand
{
  /** Authenticate a user and establish a session.
    * @param userAndPassword if None, the HTTP header Authentication is used to allow browser authentication dialog.
    */
  final case class Login(userAndPassword: Option[UserAndPassword]) extends SessionCommand {
    type Response = Login.Response
  }

  object Login {
    final case class Response(sessionToken: SessionToken) extends SessionCommand.Response

    object Response {
      implicit val jsonEncoder: ObjectEncoder[Response] =
        o ⇒ JsonObject("sessionToken" → Json.fromString(o.sessionToken.secret.string))

      implicit val jsonDecoder: Decoder[Response] =
        cursor ⇒
          for (token ← cursor.get[String]("sessionToken")) yield
            Response(SessionToken(SecretString(token)))
    }
  }

  /** Invalidate the session established by `Login`.
    */
  case object Logout extends SessionCommand {
    type Response = SessionCommand.Response.Accepted.type
  }

  sealed trait Response
  object Response {
    sealed trait Accepted extends Response
    case object Accepted extends Accepted {
      implicit val jsonCodec: CirceCodec[Accepted.type] = singletonCodec(Accepted)
    }

    implicit val jsonCodec = TypedJsonCodec[Response](
      Subtype(Response.Accepted),
      Subtype.named[Login.Response]("LoggedIn"))
  }

  implicit val jsonCodec = {
    implicit val x = UserAndPassword.jsonCodec
    TypedJsonCodec[SessionCommand](
      Subtype(deriveCodec[Login]),
      Subtype(Logout))
  }
}
