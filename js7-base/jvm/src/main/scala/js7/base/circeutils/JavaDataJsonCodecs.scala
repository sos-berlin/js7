package js7.base.circeutils

import io.circe.Decoder.Result
import io.circe.{Codec, Decoder, DecodingFailure, Encoder, HCursor, Json}
import java.time.format.DateTimeFormatter
import java.time.{Duration, Instant, ZoneId}
import scala.util.control.NonFatal

/**
  * @author Joacim Zschimmer
  */
object JavaDataJsonCodecs:

  implicit val DurationEncoder: Encoder[Duration] =
    o => o.getNano match
      case 0 => Json.fromLong(o.getSeconds)
      case nano => Json.fromBigDecimal(BigDecimal(o.getSeconds) + BigDecimal(nano, scale = 9))

  implicit val DurationDecoder: Decoder[Duration] =
    cursor =>
      cursor.as[Long] map Duration.ofSeconds orElse (
        cursor.as[BigDecimal] map bigDecimalToDuration orElse (
          cursor.as[String] map Duration.parse))

  private def bigDecimalToDuration(bigDecimal: BigDecimal) =
    val (seconds, nanos) = bigDecimal /% 1
    Duration.ofSeconds(seconds.toLongExact, (nanos * 1000*1000*1000).toIntExact)

  private val dateTimeFormatter = DateTimeFormatter.ISO_INSTANT

  object StringInstantEncoder extends Encoder[Instant]:
    def apply(o: Instant): Json = 
      Json.fromString(dateTimeFormatter.format(o))

  object NumericInstantEncoder extends Encoder[Instant]:
    def apply(o: Instant): Json = 
      Json.fromLong(o.toEpochMilli)

  trait InstantDecoder extends Decoder[Instant]:
    def apply(cursor: HCursor): Result[Instant] =
      if cursor.value.isNumber then
        cursor.as[Long] map Instant.ofEpochMilli
      else
        cursor.as[String].map(o => Instant.from(dateTimeFormatter parse o))
  object InstantDecoder extends InstantDecoder

  object instant:
    implicit val NumericInstantJsonCodec: Codec[Instant] =
      Codec.from(InstantDecoder, NumericInstantEncoder)

    implicit val StringInstantJsonCodec: Codec[Instant] =
      Codec.from(InstantDecoder, StringInstantEncoder)

    implicit val InstantJsonCodec: Codec[Instant] =
      NumericInstantJsonCodec

  implicit val zoneIdJsonEncoder: Encoder[ZoneId] =
    o => Json.fromString(o.getId)

  implicit val zoneIdJsonDecoder: Decoder[ZoneId] =
    cursor => cursor.as[String].flatMap(string =>
      try Right(ZoneId.of(string))
      catch {
        case NonFatal(e) => Left(DecodingFailure(e.toString, cursor.history))
      })
