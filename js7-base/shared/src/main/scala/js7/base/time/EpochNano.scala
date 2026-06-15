package js7.base.time

import cats.Show
import io.circe.{Decoder, Encoder, Json}
import java.time.{Instant, OffsetDateTime, ZonedDateTime}
import js7.base.utils.ScalaUtils.syntax.*
import scala.concurrent.duration.FiniteDuration

/** A Long denoting the number of nanoseconds since the Unix epoch. */
opaque type EpochNano = Long


object EpochNano:
  val Zero: EpochNano = 0L
  private inline val nix = -7_777_777_777_777_777_777L // A noticeable low number
  val NixVal: EpochNano = nix
  inline def Nix: EpochNano = nix
  private val zeroJson = Json.fromInt(0)

  inline def apply(epochNano: Long): EpochNano =
    epochNano

  inline def of(duration: FiniteDuration): EpochNano =
    duration.toNanos

  def fromDecimalString(string: String): EpochNano =
    java.math.BigDecimal(string).movePointRight(9).longValue

  given Ordering[EpochNano] = Ordering.Long

  given Show[EpochNano] = _.show

  given Encoder[EpochNano] =
    case EpochNano.Zero => zeroJson
    case epochNano => Json.fromBigDecimal:
      java.math.BigDecimal.valueOf(epochNano.toLong, 6)

  given Decoder[EpochNano] = c =>
    c.as[java.math.BigDecimal].map:
      _.movePointRight(6).longValue

  extension (self: EpochNano)
    inline def toLong: Long =
      self

    inline def nanosecondSinceSecond: Long =
      self % 1_000_000_000L

    inline def isNix: Boolean =
      self == nix

    infix def min(o: EpochNano): EpochNano =
      if isNix then
        if o.isNix then Nix else o
      else if o.isNix then Nix
      else Math.min(self, o)

    def toOption: Option[EpochNano] =
      !isNix ? self

    def toDecimalString: String =
      if self == 0L then
        "0"
      else
        val string = java.math.BigDecimal.valueOf(self, 9).toPlainString
        var i = string.length
        while string(i - 1) == '0' do i -= 1
        if string(i - 1) == '.' then i -= 1
        assert(i > 0)
        string.substring(0, i)

    def toInstant: Instant =
      Instant.ofEpochSecond(self / 1_000_000_000, (self % 1_000_000_000).toInt)

    def show: String =
      toInstant.toString
  end extension


  private val MinNanoInstant = Instant.ofEpochMilli(Long.MinValue)
  private val MaxNanoInstant = Instant.ofEpochMilli(Long.MaxValue)
  private inline val MinSecond = Long.MinValue / 1_000_000_000
  private inline val MaxSecond = Long.MaxValue / 1_000_000_000

  extension (instant: Instant)
    def toEpochNano: EpochNano =
      if instant.isBefore(MinNanoInstant) || instant.isAfter(MaxNanoInstant) then
        throw new IllegalArgumentException(s"Instant $instant is out of range for EpochNano")
      EpochNano(instant.getEpochSecond * 1_000_000_000L + instant.getNano)

  extension (offsetDateTime: OffsetDateTime)
    def toEpochNano: EpochNano =
      val second = offsetDateTime.toEpochSecond
      if second < MinSecond || second > MaxSecond then
        throw new IllegalArgumentException(s"OffsetDateTime $offsetDateTime is out of range for EpochNano")
      EpochNano(second * 1_000_000_000L + offsetDateTime.getNano)

  extension (zonedDateTime: ZonedDateTime)
    def toEpochNano: EpochNano =
      val second = zonedDateTime.toEpochSecond
      if second < MinSecond || second > MaxSecond then
        throw new IllegalArgumentException(s"ZonedDateTime $zonedDateTime is out of range for EpochNano")
      EpochNano(second * 1_000_000_000L + zonedDateTime.getNano)
