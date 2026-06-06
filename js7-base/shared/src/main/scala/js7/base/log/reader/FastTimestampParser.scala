package js7.base.log.reader

import java.lang.System.arraycopy
import java.nio.charset.StandardCharsets
import java.time.format.{DateTimeFormatter, DateTimeFormatterBuilder, DateTimeParseException}
import java.time.temporal.ChronoField.NANO_OF_SECOND
import java.time.{LocalDateTime, OffsetDateTime, ZoneId}
import js7.base.data.ByteSequence.ops.*
import js7.base.data.{ByteArray, ByteSequence}
import js7.base.log.Logger
import js7.base.log.reader.FastTimestampParser.*
import js7.base.time.EpochNano
import js7.base.time.EpochNano.toEpochNano
import js7.base.utils.ScalaUtils.syntax.RichThrowable
import scala.annotation.switch
import scala.util.matching.Regex

/** Fast parser for reading a logging file's timestamps — no concurrent use!
  *
  * Optimized for consecutive calls with timestamps in the same second.
  *
  * Only local timestamps are parsed.
  */
final class FastTimestampParser()(using zoneId: ZoneId):

  private val lastSecond = new Array[Byte](SecondLength)
  private var lastEpochSecond = 0L
  private val lastZone = new Array[Byte](ZoneSize)
  private val ts = new Array[Byte](LongestLength)

  assert(lastSecond.length == SecondLength)

  def parse(timestamp: String): EpochNano =
    parse(ByteArray(timestamp))

  def parse[ByteSeq: ByteSequence](timestamp: ByteSeq): EpochNano =
    parse(timestamp, 0, timestamp.length)

  /** @return EpochNano.Nix if timestamp is invalid .*/
  def parse[ByteSeq: ByteSequence](timestampBytes: ByteSeq, start: Int, end: Int): EpochNano =
    val length = end - start
    if length < MinimumLength || length > LongestLength then
      return EpochNano.Nix

    timestampBytes.copyToArray(start, ts, 0, length)
    ts(10) = 'T' // Replace ' ' with 'T'
    ts(19) = '.' // Replace ',' with '.'

    var zonePos =
      if ts(end - 1) == 'Z' then end - 1
      else if ts(end - 3) <= '-' then end - 3 // "+02"
      else if ts(end - 5) <= '-' then end - 5 // "+0230"
      else if ts(end - 6) <= '-' then end - 6 // "+02:30"
      else end // No zone offset, local time

    def isLastZone: Boolean =
      var i = 0
      var j = zonePos
      var end_ = end - zonePos
      while i < end_ do
        if lastZone(i) != ts(j).toByte then return false
        i += 1
        j += 1
      i == ZoneSize || lastZone(i) == ' '

    if java.util.Arrays.equals(ts, 0, SecondLength, lastSecond, 0, SecondLength)
      && isLastZone
    then
      // Same second and zone
      val fraction: Int =
        (zonePos: @switch) match
          case 19 | 20 => 0
          case 21 =>
            (ts(20) - '0') * 100000000
          case 22 =>
            (ts(20) - '0') * 100000000 +
            (ts(21) - '0') * 10000000
          case 23 =>
            (ts(20) - '0') * 100000000 +
            (ts(21) - '0') * 10000000 +
            (ts(22) - '0') * 1000000
          case 24 =>
            (ts(20) - '0') * 100000000 +
            (ts(21) - '0') * 10000000 +
            (ts(22) - '0') * 1000000 +
            (ts(23) - '0') * 100000
          case 25 =>
            (ts(20) - '0') * 100000000 +
            (ts(21) - '0') * 10000000 +
            (ts(22) - '0') * 1000000 +
            (ts(23) - '0') * 100000 +
            (ts(24) - '0') * 10000
          case 26 =>
            (ts(20) - '0') * 100000000 +
            (ts(21) - '0') * 10000000 +
            (ts(22) - '0') * 1000000 +
            (ts(23) - '0') * 100000 +
            (ts(24) - '0') * 10000 +
            (ts(25) - '0') * 1000
          case 27 =>
            (ts(20) - '0') * 100000000 +
            (ts(21) - '0') * 10000000 +
            (ts(22) - '0') * 1000000 +
            (ts(23) - '0') * 100000 +
            (ts(24) - '0') * 10000 +
            (ts(25) - '0') * 1000 +
            (ts(26) - '0') * 100
          case 28 =>
            (ts(20) - '0') * 100000000 +
            (ts(21) - '0') * 10000000 +
            (ts(22) - '0') * 1000000 +
            (ts(23) - '0') * 100000 +
            (ts(24) - '0') * 10000 +
            (ts(25) - '0') * 1000 +
            (ts(26) - '0') * 100 +
            (ts(27) - '0') * 10
          case 29 =>
            (ts(20) - '0') * 100000000 +
            (ts(21) - '0') * 10000000 +
            (ts(22) - '0') * 1000000 +
            (ts(23) - '0') * 100000 +
            (ts(24) - '0') * 10000 +
            (ts(25) - '0') * 1000 +
            (ts(26) - '0') * 100 +
            (ts(27) - '0') * 10 +
            (ts(28) - '0')
      EpochNano(lastEpochSecond + fraction)
    else
      // Different second or zone
      try
        val epochNano = parseTimestampAsNanos(new String(ts, 0, timestampBytes.length, StandardCharsets.US_ASCII))

        // Safe lastSecond
        arraycopy(ts, 0, lastSecond, 0, lastSecond.length)
        lastEpochSecond = epochNano.toLong / 1_000_000_000 * 1_000_000_000

        // Save lastZone
        arraycopy(ts, zonePos, lastZone, 0, end - zonePos)
        java.util.Arrays.fill(lastZone, end - zonePos, 6, ' '.toByte)

        epochNano
      catch case e: DateTimeParseException =>
        logger.trace("💥 " + e.toStringWithCauses)
        EpochNano.Nix


object FastTimestampParser:
  private val logger = Logger[FastTimestampParser]
  private inline val LongestLength = 35
  private inline val SecondLength = 19
  private inline val MinimumLength = SecondLength
  private inline val ZoneSize = 6 // "+01:00"

  assert(LongestLength == "0000-00-00T00:00:00.000000000+00:00".length)
  assert(SecondLength == "0000-00-00T00:00:00".length)

  val DateTimeRegex: Regex =
    """\d{4}-\d{2}-\d{2}[ T]\d{2}:\d{2}:\d{2}[.,]\d{1,9}(Z|[+-][0-9:]{2,5})?""".r

  // Faster than ISO_LOCAL_DATE_TIME
  private val dateTimeFormatter: DateTimeFormatter =
    new DateTimeFormatterBuilder()
      .appendPattern("yyyy-MM-dd'T'HH:mm:ss")
      .appendFraction(NANO_OF_SECOND, 0, 9, true)
      .appendOptional(new DateTimeFormatterBuilder().appendOffset("+HHMM", "Z").toFormatter)
      .appendOptional(new DateTimeFormatterBuilder().appendOffset("+HH:mm", "Z").toFormatter)
      .toFormatter

  /** @throws IllegalArgumentException
    * @throws DateTimeParseException
    */
  def parseTimestampAsNanos(string: CharSequence)(using zoneId: ZoneId): EpochNano =
    dateTimeFormatter.parseBest(string, OffsetDateTime.from, LocalDateTime.from) match
      case o: OffsetDateTime => o.toEpochNano
      case o: LocalDateTime => o.atZone(zoneId).toEpochNano
