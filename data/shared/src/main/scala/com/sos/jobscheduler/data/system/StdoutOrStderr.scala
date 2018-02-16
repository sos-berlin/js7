package com.sos.jobscheduler.data.system

import com.sos.jobscheduler.base.generic.IsString
import io.circe.{Decoder, DecodingFailure, Encoder, Json, KeyDecoder, KeyEncoder}

/**
 * @author Joacim Zschimmer
 */
sealed trait StdoutOrStderr extends IsString
with Product with Serializable/*to be compatible with case object*/ {
  val string: String
  override def toString = string
}

object StdoutOrStderr {
  val values = List(Stdout, Stderr)

  implicit val keyJsonEncoder: KeyEncoder[StdoutOrStderr] = _.string
  implicit val keyJsonDecoder: KeyDecoder[StdoutOrStderr] = {
    case Stdout.string ⇒ Some(Stdout)
    case Stderr.string ⇒ Some(Stderr)
    case _ ⇒ None
  }
  implicit val jsonEncoder: Encoder[StdoutOrStderr] = o ⇒ Json.fromString(o.string)
  implicit val jsonDecoder: Decoder[StdoutOrStderr] = _.as[String] flatMap { string ⇒
    keyJsonDecoder(string) match {
      case Some(o) ⇒ Right(o)
      case None ⇒ Left(DecodingFailure(s"'stdout' or 'stderr' expected, not: $string", Nil))
    }
  }
}

case object Stdout extends StdoutOrStderr with IsString {
  val string = "stdout"
}

case object Stderr extends StdoutOrStderr with IsString {
  val string = "stderr"
}
