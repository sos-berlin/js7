package js7.base.time

import cats.Show
import java.time.*
import java.util.Date
import js7.base.problem.Checked
import js7.base.time.JavaTimeConverters.AsScalaDuration
import js7.base.time.ScalaTime.*
import org.jetbrains.annotations.TestOnly
import scala.concurrent.duration.FiniteDuration
import scala.math.Ordered.orderingToOrdered
import scala.util.matching.Regex

object JavaTime:

  private val MaxDuration = Duration.ofSeconds(Long.MaxValue, 999999999)
  private val MinDuration = Duration.ofSeconds(Long.MinValue, 999999999)
  val Iso8601DurationRegex: Regex = "[0-9.A-Za-z]+".r

  @TestOnly @volatile var extraSleepCount = 0L

  given Ordering[Duration] = (a, b) => a.compareTo(b)
  given Ordering[Instant] = (a, b) => a.compareTo(b)
  given Ordering[LocalDateTime] = (a, b) => a.compareTo(b)

  given Show[Date] =
    Show[java.util.Date](using o =>
      JavaTimestamp(o).show)

  def bigDecimalToDuration(o: BigDecimal): Duration =
    val (seconds, fraction) = o /% 1
    try Duration.ofSeconds(seconds.toLongExact, (fraction * 1000 * 1000 * 1000).toIntExact)
    catch case t: ArithmeticException =>
      throw new ArithmeticException(s"Value '$o' is not a Duration: ${Option(t.getMessage) getOrElse t}")


  object extensions:
    //extension (delegate: Int)
    //  def *(o: Duration): Duration =
    //    o.multipliedBy(delegate)
    //
    //extension (delegate: Long)
    //  def *(o: Duration): Duration =
    //    o.multipliedBy(delegate)
    //
    extension (duration: Duration)
      def unary_- : Duration =
        Duration.ZERO.minus(duration)

      def +(o: Duration): Duration =
        duration.plus(o)

      def -(o: Duration): Duration =
        duration.minus(o)

      def *(o: Long): Duration =
        duration.multipliedBy(o)

      def /(o: Long): Duration =
        duration.dividedBy(o)

      def *(o: BigDecimal): Duration =
        bigDecimalToDuration(duration.toBigDecimal * o)

      def /(o: BigDecimal): Duration =
        bigDecimalToDuration(duration.toBigDecimal / o)

      def min(o: Duration): Duration =
        if duration <= o then duration else o

      def max(o: Duration): Duration =
        if duration > o then duration else o

      def isPositive: Boolean =
        duration >= Duration.ZERO

      def toBigDecimal: BigDecimal =
        BigDecimal(duration.getSeconds) + BigDecimal(duration.getNano) / (1000*1000*1000)

      def pretty: String =
        if (duration > MaxDuration)  || (duration < MinDuration) then
          duration.toString
        else
          duration.toFiniteDuration.pretty


    extension (instant: Instant)
      def +(o: Duration): Instant =
        instant.plus(o)

      def -(o: Duration): Instant =
        instant.minus(o)

      def -(o: Instant): Duration =
        Duration.between(o, instant)

      def roundTo(duration: Duration): Instant =
        (instant + duration / 2).roundDownTo(duration)

      def roundDownTo(duration: Duration): Instant =
        val durationMillis = duration.toMillis
        Instant.ofEpochMilli(instant.toEpochMilli / durationMillis * durationMillis)


    extension(localDateTime: LocalDateTime)
      def +(o: Duration): LocalDateTime =
        localDateTime.plus(o)

      def +(o: FiniteDuration): LocalDateTime =
        localDateTime.plusNanos(o.toNanos)

      def -(o: Duration): LocalDateTime =
        localDateTime.minus(o)

      def -(o: FiniteDuration): LocalDateTime =
        localDateTime.minusNanos(o.toNanos)

      def toInstant(zoneId: ZoneId): Instant =
        ZonedDateTime.of(localDateTime, zoneId).toInstant

      def normalize(using zoneId: ZoneId): LocalDateTime =
        LocalDateTime.ofInstant(localDateTime.toInstant(zoneId), zoneId)


    extension(timeZone: Timezone)
      def toZoneId: Checked[ZoneId] =
        Checked.catchExpected[RuntimeException](ZoneId.of(timeZone.string))
