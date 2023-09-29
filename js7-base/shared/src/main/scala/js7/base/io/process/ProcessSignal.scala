package js7.base.io.process

import io.circe.{Decoder, DecodingFailure, Encoder, Json}

/**
  * @author Joacim Zschimmer
  */
sealed abstract class ProcessSignal(val number: Int)

object ProcessSignal:
  case object SIGTERM extends ProcessSignal(15)
  case object SIGKILL extends ProcessSignal(9)

  implicit val jsonEncoder: Encoder[ProcessSignal] =
    o => Json.fromString(o.toString)

  implicit val jsonDecoder: Decoder[ProcessSignal] =
    cursor => cursor.as[String].flatMap:
      case "SIGTERM" => Right(SIGTERM)
      case "SIGKILL" => Right(SIGKILL)
      case signal => Left(DecodingFailure(s"Invalid process signal: $signal (SIGTERM or SIGKILL expected)", cursor.history))
