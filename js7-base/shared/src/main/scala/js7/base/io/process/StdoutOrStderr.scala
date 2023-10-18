package js7.base.io.process

import io.circe.{Decoder, DecodingFailure, Encoder, Json, KeyDecoder, KeyEncoder}
import js7.base.annotation.javaApi
import js7.base.generic.GenericString

/**
 * @author Joacim Zschimmer
 */
sealed trait StdoutOrStderr extends GenericString, Product, Serializable/*to be compatible with case object*/ :
  
  val string: String
  override def toString = string


object StdoutOrStderr:
  val values = List(Stdout, Stderr)

  implicit val keyJsonEncoder: KeyEncoder[StdoutOrStderr] = _.string
  implicit val keyJsonDecoder: KeyDecoder[StdoutOrStderr] =
    case Stdout.string => Some(Stdout)
    case Stderr.string => Some(Stderr)
    case _ => None
  implicit val jsonEncoder: Encoder[StdoutOrStderr] = o => Json.fromString(o.string)
  implicit val jsonDecoder: Decoder[StdoutOrStderr] = cursor => cursor.as[String] flatMap { string =>
    keyJsonDecoder(string) match
      case Some(o) => Right(o)
      case None => Left(DecodingFailure(s"'stdout' or 'stderr' expected, not: $string", cursor.history))
  }

case object Stdout extends StdoutOrStderr, GenericString:
  @javaApi
  val singleton = this

  val string = "stdout"

case object Stderr extends StdoutOrStderr, GenericString:
  @javaApi
  val singleton = this

  val string = "stderr"
