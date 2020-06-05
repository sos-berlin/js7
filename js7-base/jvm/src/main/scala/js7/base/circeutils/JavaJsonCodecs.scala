package js7.base.circeutils

import io.circe.{Decoder, DecodingFailure, Encoder, HCursor, Json}
import java.nio.file.{Path, Paths}
import java.time.format.DateTimeFormatter
import java.time.{Duration, Instant, ZoneId}
import js7.base.circeutils.CirceUtils.toStringJsonCodec
import scala.util.control.NonFatal

/**
  * @author Joacim Zschimmer
  */
object JavaJsonCodecs
{
  implicit val PathJsonCodec: CirceCodec[Path] = toStringJsonCodec(o => Paths.get(o))

  implicit val DurationEncoder: Encoder[Duration] =
    o => o.getNano match {
      case 0 => Json.fromLong(o.getSeconds)
      case nano => Json.fromBigDecimal(BigDecimal(o.getSeconds) + BigDecimal(nano, scale = 9))
    }

  implicit val DurationDecoder: Decoder[Duration] =
    cursor =>
      cursor.as[Long] map Duration.ofSeconds orElse (
        cursor.as[BigDecimal] map bigDecimalToDuration orElse (
          cursor.as[String] map Duration.parse))

  private def bigDecimalToDuration(bigDecimal: BigDecimal) = {
    val (seconds, nanos) = bigDecimal /% 1
    Duration.ofSeconds(seconds.toLongExact, (nanos * 1000*1000*1000).toIntExact)
  }

  private val dateTimeFormatter = DateTimeFormatter.ISO_INSTANT

  trait StringInstantEncoder extends Encoder[Instant] {
    def apply(o: Instant) = Json.fromString(dateTimeFormatter.format(o))
  }
  object StringInstantEncoder extends StringInstantEncoder

  trait NumericInstantEncoder extends Encoder[Instant] {
    def apply(o: Instant) = Json.fromLong(o.toEpochMilli)
  }
  object NumericInstantEncoder extends NumericInstantEncoder

  trait InstantDecoder extends Decoder[Instant] {
    def apply(cursor: HCursor) =
      if (cursor.value.isNumber)
        cursor.as[Long] map Instant.ofEpochMilli
      else
        cursor.as[String] map (o => Instant.from(dateTimeFormatter parse o))
  }
  object InstantDecoder extends InstantDecoder

  object instant {
    implicit val NumericInstantJsonCodec: CirceCodec[Instant] = new NumericInstantEncoder with InstantDecoder
    implicit val StringInstantJsonCodec: CirceCodec[Instant] = new StringInstantEncoder with InstantDecoder

    implicit val InstantJsonCodec = NumericInstantJsonCodec
  }

  implicit val zoneIdJsonEncoder: Encoder[ZoneId] =
    o => Json.fromString(o.getId)

  implicit val zoneIdJsonDecoder: Decoder[ZoneId] =
    cursor => cursor.as[String] flatMap (string =>
      try Right(ZoneId.of(string))
      catch {
        case NonFatal(e) => Left(DecodingFailure(e.toString, cursor.history))
      })
}
