package js7.base.time

import cats.Show
import java.time.*
import js7.base.problem.Checked
import js7.base.time.JavaTimeConverters.AsScalaDuration
import js7.base.time.ScalaTime.RichFiniteDuration
import org.jetbrains.annotations.TestOnly
import scala.concurrent.duration.FiniteDuration

object JavaTime
{
  private val MaxDuration = Duration.ofSeconds(Long.MaxValue, 999999999)
  private val MinDuration = Duration.ofSeconds(Long.MinValue, 999999999)
  val Iso8601DurationRegex = "[0-9.A-Za-z]+".r

  @TestOnly @volatile var extraSleepCount = 0L

  implicit final class JavaDurationRichInt(private val delegate: Int) extends AnyVal {
    def *(o: Duration) =  o multipliedBy delegate
  }

  implicit final class JavaDurationRichLong(private val delegate: Long) extends AnyVal {
    def *(o: Duration) = o multipliedBy delegate
  }

  def bigDecimalToDuration(o: BigDecimal): Duration = {
    val (seconds, fraction) = o /% 1
    try Duration.ofSeconds(seconds.toLongExact, (fraction * 1000*1000*1000).toIntExact)
    catch { case t: ArithmeticException =>
      throw new ArithmeticException(s"Value '$o' is not a Duration: ${Option(t.getMessage) getOrElse t}")
    }
  }

  implicit final class RichDuration(private val delegate: Duration)
  extends AnyVal with Ordered[RichDuration] {
    def unary_- = Duration.ZERO minus delegate
    def +(o: Duration): Duration = delegate plus o
    def -(o: Duration): Duration = delegate minus o
    def *(o: Long): Duration = delegate multipliedBy o
    def /(o: Long): Duration = delegate dividedBy o
    def *(o: BigDecimal): Duration = bigDecimalToDuration(delegate.toBigDecimal * o)
    def /(o: BigDecimal): Duration = bigDecimalToDuration(delegate.toBigDecimal / o)
    def min(o: Duration): Duration = if (this <= o) delegate else o
    def max(o: Duration): Duration = if (this > o) delegate else o

    def isPositive = delegate >= Duration.ZERO

    def toBigDecimal = BigDecimal(delegate.getSeconds) + BigDecimal(delegate.getNano) / (1000*1000*1000)
    override def toString = pretty  // For ScalaTest

    def pretty: String =
      if ((delegate > MaxDuration)  || (delegate < MinDuration))
        delegate.toString
      else
        delegate.toFiniteDuration.pretty

    def compare(o: RichDuration) = delegate compareTo o.delegate
  }

  implicit final class RichInstant(private val delegate: Instant) extends AnyVal with Ordered[RichInstant] {
    def +(o: Duration) = delegate plus o
    def -(o: Duration) = delegate minus o
    def -(o: Instant) = Duration.between(o, delegate)
    def min(o: Instant): Instant = if (this <= o) delegate else o
    def max(o: Instant): Instant = if (this > o) delegate else o
    def compare(o: RichInstant) = delegate compareTo o.delegate

    def roundTo(duration: Duration): Instant = this + duration / 2 roundDownTo duration

    def roundDownTo(duration: Duration): Instant = {
      val durationMillis = duration.toMillis
      Instant.ofEpochMilli(delegate.toEpochMilli / durationMillis * durationMillis)
    }

    override def toString = delegate.toString  // For ScalaTest
  }

  implicit final class RichLocalTime(private val delegate: LocalTime) extends AnyVal with Ordered[RichLocalTime] {
    def compare(o: RichLocalTime) = delegate compareTo o.delegate
  }

  implicit final class RichLocalDateTime(private val localDateTime: LocalDateTime)
  extends AnyVal with Ordered[RichLocalDateTime]
  {
    def +(o: Duration): LocalDateTime = localDateTime plus o
    def +(o: FiniteDuration): LocalDateTime = localDateTime.plusNanos(o.toNanos)
    def -(o: Duration): LocalDateTime = localDateTime minus o
    def -(o: FiniteDuration): LocalDateTime = localDateTime.minusNanos(o.toNanos)
    def min(o: LocalDateTime): LocalDateTime = if (this <= o) localDateTime else o
    def max(o: LocalDateTime): LocalDateTime = if (this > o) localDateTime else o
    def compare(o: RichLocalDateTime) = localDateTime compareTo o.localDateTime
    def toInstant(zone: ZoneId) = ZonedDateTime.of(localDateTime, zone).toInstant
  }

  implicit final class JavaTimeZone(private val timeZone: Timezone) extends AnyVal
  {
    def toZoneId: Checked[ZoneId] =
      Checked.catchNonFatal(ZoneId.of(timeZone.string))
  }

  def dateToInstant(date: java.util.Date): Instant = Instant.ofEpochMilli(date.getTime)

  implicit val JavaUtilDateShow = Show[java.util.Date](o =>
    JavaTimestamp(o).show)
}
