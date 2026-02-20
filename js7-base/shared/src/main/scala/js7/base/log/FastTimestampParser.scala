package js7.base.log

import java.lang.System.arraycopy
import java.nio.CharBuffer
import java.time.format.{DateTimeFormatter, DateTimeParseException}
import java.time.{LocalDateTime, ZoneId}
import js7.base.log.FastTimestampParser.*
import js7.base.metering.CallMeter
import org.jetbrains.annotations.TestOnly
import js7.base.system.Java17Polyfill.getChars
import js7.base.utils.JavaExtensions.toEpochNanos

/** Optimized for multiple timestamp in same second - no concurrent use! * */
private final class FastTimestampParser(zoneId: ZoneId):

  private val lastSecond = "0000-00-00T00:00:00".toCharArray
  private var lastEpochSecond = 0L
  private val ts = new Array[Char](26)

  @TestOnly
  def apply(timestampString: CharSequence): Long =
    apply(timestampString, 0, timestampString.length)

  /** @return -1 if timestamp is invalid .*/
  def apply(timestampString: CharSequence, start: Int, end: Int): Long =
    meterTimestampParser:
      // Fill fraction of seconds, to allow timestamps with precision below microseconds
      ts(20) = '0'
      ts(21) = '0'
      ts(22) = '0'
      ts(23) = '0'
      ts(24) = '0'
      ts(25) = '0'
      timestampString.getChars(start, end, ts, 0)
      ts(10) = 'T'
      ts(19) = '.'

      if java.util.Arrays.equals(ts, 0, 19, lastSecond, 0, 19) then
        // Same second
        lastEpochSecond +
          (ts(20) - '0') * 100000000L +
          (ts(21) - '0') * 10000000L +
          (ts(22) - '0') * 1000000L +
          (ts(23) - '0') * 100000L +
          (ts(24) - '0') * 10000L +
          (ts(25) - '0') * 1000L
      else
        try
          // Different second
          val epochNano = parseTimestampAsNanos(CharBuffer.wrap(ts), zoneId)
          arraycopy(ts, 0, lastSecond, 0, lastSecond.length)
          lastEpochSecond = epochNano / 1_000_000_000L * 1_000_000_000L
          epochNano
        catch case _: DateTimeParseException =>
          -1


private object FastTimestampParser:
  val dateTimeFormatter: DateTimeFormatter =
    DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH:mm:ss.SSSSSS")

  private val meterTimestampParser = CallMeter("FastTimestampParser")

  private[log] def parseTimestampAsNanos(timestamp: CharSequence, zoneId: ZoneId) =
    LocalDateTime.parse(timestamp, FastTimestampParser.dateTimeFormatter)
      .atZone(zoneId)
      .toInstant
      .toEpochNanos
