package js7.base.time

import io.circe.{Decoder, Encoder, Json}
import java.time.Instant

opaque type EpochNano = Long


object EpochNano:
  val Zero: EpochNano = 0L
  val Nix: EpochNano = Long.MinValue
  private val zeroJson = Json.fromInt(0)

  inline def apply(epochNano: Long): EpochNano = epochNano

  def from(instant: Instant) =
    instant.getEpochSecond * 1_000_000_000L + instant.getNano

  extension (epochNano: EpochNano)
    inline def second: Long =
      epochNano / 1_000_000_000L * 1_000_000_000L

    inline def nanosecondSinceSecond: Long =
      epochNano % 1_000_000_000L

    def toDecimalString: String =
      if epochNano == 0L then
        "0"
      else
        java.math.BigDecimal.valueOf(epochNano, 9).toString

    inline def <(other: EpochNano): Boolean =
      epochNano < other

    def toInstant: Instant =
      Instant.ofEpochSecond(
        epochNano / 1_000_000_000,
        (epochNano % 1_000_000_000).toInt)

  given Encoder[EpochNano] =
    case EpochNano.Zero => zeroJson
    case epochNano => Json.fromBigDecimal:
      java.math.BigDecimal.valueOf(epochNano.toLong, 9)

  given Decoder[EpochNano] = c =>
    c.as[java.math.BigDecimal].map:
      _.movePointRight(9).longValue
