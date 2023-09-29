package js7.provider.scheduledorder.oldruntime

import java.time.*
import java.util.NoSuchElementException
import js7.base.time.JavaTime.*
import js7.provider.scheduledorder.oldruntime.OldSchedule.*
import scala.annotation.tailrec
import scala.collection.AbstractIterator

final case class OldSchedule(
  timeZone: ZoneId,
  weekdays: PartialFunction[DayOfWeek, PeriodSeq])
  //startOnce: Boolean = false)
extends Schedule:

  def firstInstant(from: Instant): Option[Instant] =
    firstInstant(InstantInterval(from, PredictionLimit))

  def firstInstant(instantInterval: InstantInterval): Option[Instant] =
    instants(instantInterval, limit = 1).buffered.headOption

  def instants(instantInterval: InstantInterval, limit: Int = Int.MaxValue): Iterator[Instant] =
    new AbstractIterator[Instant]:
      private var remaining = limit
      private var from = instantInterval.from minusNanos 1
      private var _next: Instant = null

      def hasNext =
        if _next == null then
          _next = find()
        _next != null

      def next() =
        if !hasNext then throw new NoSuchElementException
        val result = _next
        _next = null
        result

      @tailrec private def find(): Instant =
        if remaining > 0 && from < instantInterval.until then
          val local = LocalDateTime.ofInstant(from, timeZone)
          periodSeq(local.toLocalDate).flatMap(_.nextLocalTime(local.toLocalTime))
            .map(_.atDate(local.toLocalDate).toInstant(timeZone))
            match
              case Some(o) if o < instantInterval.until =>
                remaining -= 1
                from = o plusNanos 1
                o
              case Some(_) =>
                null
              case None =>
                from = local.toLocalDate.plusDays(1).atStartOfDay.toInstant(timeZone)
                find()
        else
          null

  private def periodSeq(date: LocalDate) =
    weekdays.lift(date.getDayOfWeek)

object OldSchedule:
  private val PredictionLimit = Duration.ofDays(366)

  def empty(timeZone: ZoneId): OldSchedule =
    OldSchedule(timeZone, Map())

  def daily(timeZone: ZoneId, periodSeq: PeriodSeq/*, startOnce: Boolean = false*/) =
    new OldSchedule(timeZone, EveryDay(periodSeq)/*, startOnce*/)

  /**
    * Unlike bare PartialFunction, EveryDay implements toString and equals
    */
  final case class EveryDay(periodSeq: PeriodSeq) extends PartialFunction[DayOfWeek, PeriodSeq]:
    def isDefinedAt(dayOfWeek: DayOfWeek) = true

    override def apply(dayOfWeek: DayOfWeek) = periodSeq

    override def toString = s"EveryDay($periodSeq)"
