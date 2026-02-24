package js7.base.log

import java.lang.System.arraycopy
import java.nio.CharBuffer
import java.time.format.{DateTimeFormatter, DateTimeParseException}
import java.time.{LocalDateTime, ZoneId}
import js7.base.log.FastTimestampParser.*
import js7.base.metering.CallMeter
import js7.base.system.Java17Polyfill.getChars
import js7.base.utils.JavaExtensions.toEpochNanos

/** Optimized for multiple timestamp in same second - no concurrent use! * */
final class FastTimestampParser(zoneId: ZoneId):

  private val lastSecond = "0000-00-00T00:00:00".toCharArray
  private var lastEpochSecond = 0L
  private val initial = "0000-00-00T00:00:00.000000      ".toCharArray
  private val ts = new Array[Char](initial.length)

  assert(lastSecond.length == 19)

  def apply(timestampString: CharSequence): Long =
    apply(timestampString, 0, timestampString.length)

  /** @return -1 if timestamp is invalid .*/
  def apply(timestampString: CharSequence, start: Int, end: Int): Long =
    meterTimestampParser:
      // Fill fraction of seconds, to allow timestamps with precision below microseconds
      System.arraycopy(initial, 0, ts, 0, ts.length)
      //ts(20) = '0'
      //ts(21) = '0'
      //ts(22) = '0'
      //ts(23) = '0'
      //ts(24) = '0'
      //ts(25) = '0'
      //ts(26) = ' '
      //ts(27) = ' '
      //ts(28) = ' '
      //ts(29) = ' '
      //ts(30) = ' '
      //ts(31) = ' '
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
      else if ts(0) == ' ' then
        -1
      else
        try
          // Different second
          var len = ts.length
          while ts(len - 1) == ' ' do len -= 1
          val epochNano = parseTimestampAsNanos(CharBuffer.wrap(ts, 0, len), zoneId)
          arraycopy(ts, 0, lastSecond, 0, lastSecond.length)
          lastEpochSecond = epochNano / 1_000_000_000L * 1_000_000_000L
          epochNano
        catch case _: DateTimeParseException =>
          -1

object FastTimestampParser:
  val dateTimeFormatter: DateTimeFormatter =
    DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH:mm:ss.SSSSSS")

  private val meterTimestampParser = CallMeter("FastTimestampParser")

  def parseTimestampAsNanos(string: CharSequence, zoneId: ZoneId) =
    LocalDateTime.parse(string, FastTimestampParser.dateTimeFormatter)
      .atZone(zoneId)
      .toInstant
      .toEpochNanos
