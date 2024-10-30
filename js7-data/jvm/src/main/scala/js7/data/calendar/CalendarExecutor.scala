package js7.data.calendar

import java.time.LocalTime.MIDNIGHT
import java.time.format.{DateTimeFormatter, DateTimeFormatterBuilder}
import java.time.temporal.IsoFields.{WEEK_BASED_YEAR, WEEK_OF_WEEK_BASED_YEAR}
import java.time.temporal.{TemporalAccessor, TemporalQueries, TemporalQuery, WeekFields}
import java.time.{LocalDate, LocalDateTime, ZoneId, ZonedDateTime}
import java.util.Locale.ROOT
import java.util.regex.PatternSyntaxException
import js7.base.problem.Checked.{CheckedOption, catchExpected}
import js7.base.problem.{Checked, Problem}
import js7.base.time.JavaTime.JavaTimeZone
import js7.base.time.JavaTimeConverters.RichZonedDateTime
import js7.base.time.ScalaTime.*
import js7.base.time.{TimeInterval, Timezone}
import js7.base.utils.ScalaUtils.syntax.*
import js7.data.order.OrderId
import scala.concurrent.duration.*
import scala.util.matching.Regex

final class CalendarExecutor private(
  calendar: Calendar,
  zone: ZoneId,
  orderIdToDateRegex: Regex,
  period: CalendarExecutor.Period):

  private val offsetSeconds = calendar.dateOffset.toSeconds

  def orderIdToTimeInterval(orderId: OrderId): Checked[TimeInterval] =
    Checked.catchNonFatalFlatten:
      for localDateTime <- orderIdToLocalDate(orderId) yield
        val zoned = localDateTime.plusSeconds(offsetSeconds).atZone(zone)
        val begin = zoned.toTimestamp
        val end = period.next(zoned).toTimestamp
        TimeInterval(begin, end - begin)

  private def orderIdToLocalDate(orderId: OrderId): Checked[LocalDateTime] =
    orderIdToDateRegex.unapplySeq(orderId.string)
      .flatMap(matches => (matches.lengthIs == 1) ? matches.head)
      .map(period.formatter.parse)
      .flatMap(toLocalDateTime)
      .toChecked(Problem.pure(s"$orderId does match the pattern defined in ${calendar.path}"))

  private def toLocalDateTime(temporalAccessor: TemporalAccessor): Option[LocalDateTime] =
    Option(temporalAccessor.query(period.query))
      .map(localDate => LocalDateTime.of(localDate, MIDNIGHT))


object CalendarExecutor:
  def checked(calendar: Calendar, timezone: Timezone): Checked[CalendarExecutor] =
    for
      period <- toPeriod(calendar.periodDatePattern)
      _ <- (calendar.dateOffset >= 0.s && calendar.dateOffset < period.normalDuration) !!
        Problem("Invalid dateOffset")
      zoneId <- timezone.toZoneId
      orderIdToDateRegex <- catchExpected[PatternSyntaxException]:
        calendar.orderIdPattern.r
    yield
      CalendarExecutor(calendar, zoneId, orderIdToDateRegex, period)

  private def toPeriod(pattern: String): Checked[Period] =
    catchExpected[Exception]:
      if pattern.startsWith("Y"/*week based year*/) then
        Weekly:
          new DateTimeFormatterBuilder()
            .appendPattern(pattern)
            .parseDefaulting(WeekFields.ISO.dayOfWeek, 1)
            .toFormatter
      else
        Daily(DateTimeFormatter.ofPattern(pattern))


  private sealed trait Period:
    protected val formatter_ : DateTimeFormatter
    lazy val formatter = formatter_.withLocale(ROOT)
    def normalDuration: FiniteDuration
    def next(zoned: ZonedDateTime): ZonedDateTime
    def query: TemporalQuery[LocalDate]


  private final case class Daily(protected val formatter_ : DateTimeFormatter)
  extends Period:
    val normalDuration = 24.h
    val query = TemporalQueries.localDate
    def next(zoned: ZonedDateTime) = zoned.plusDays(1)


  private final case class Weekly(protected val formatter_ : DateTimeFormatter)
  extends Period:
    val normalDuration = 7 * 24.h
    val query = DateOfWeekBasedYearQuery
    def next(zoned: ZonedDateTime) = zoned.plusDays(7)


  private object DateOfWeekBasedYearQuery extends TemporalQuery[LocalDate]():
    def queryFrom(temporal: TemporalAccessor): LocalDate | Null =
      if !temporal.isSupported(WEEK_OF_WEEK_BASED_YEAR)
        || !temporal.isSupported(WEEK_BASED_YEAR) then
        null // Not found
      else
        val week = temporal.get(WEEK_OF_WEEK_BASED_YEAR)
        val year = temporal.get(WEEK_BASED_YEAR)
        // Could this be calculated easier??
        //val newYear = LocalDate.of(year, 1, 1)
        //ZonedDateTime.of(newYear, MIDNIGHT, UTC).getDayOfWeek.getValue
        DateTimeFormatter.ISO_WEEK_DATE
          .parse(f"$year%04d-W$week%02d-1")
          .query(TemporalQueries.localDate)

    override def toString = "DateOfWeekBasedYearQuery"
