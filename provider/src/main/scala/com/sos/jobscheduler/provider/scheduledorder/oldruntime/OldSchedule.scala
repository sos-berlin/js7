package com.sos.jobscheduler.provider.scheduledorder.oldruntime

import com.google.common.collect.{AbstractIterator => GuavaIterator}
import com.sos.jobscheduler.common.time.JavaTime._
import com.sos.jobscheduler.provider.scheduledorder.oldruntime.OldSchedule._
import java.time._
import scala.annotation.tailrec
import scala.jdk.CollectionConverters._

final case class OldSchedule(
  timeZone: ZoneId,
  weekdays: PartialFunction[DayOfWeek, PeriodSeq])
  //startOnce: Boolean = false)
extends Schedule {

  def firstInstant(from: Instant): Option[Instant] =
    firstInstant(InstantInterval(from, PredictionLimit))

  def firstInstant(instantInterval: InstantInterval): Option[Instant] =
    instants(instantInterval, limit = 1).buffered.headOption

  def instants(instantInterval: InstantInterval, limit: Int = Int.MaxValue): Iterator[Instant] =
    new GuavaIterator[Instant] {
      private var remaining = limit
      private var from = instantInterval.from minusNanos 1

      def computeNext() = {
        @tailrec def find(): Instant = {
          if (remaining > 0 && from < instantInterval.until) {
            val local = LocalDateTime.ofInstant(from, timeZone)
            periodSeq(local.toLocalDate) flatMap { _.nextLocalTime(local.toLocalTime) } map {
              _.atDate(local.toLocalDate).toInstant(timeZone)
            } match {
              case Some(o) if o < instantInterval.until =>
                remaining -= 1
                from = o plusNanos 1
                o
              case Some(_) =>
                endOfData
              case None =>
                from = local.toLocalDate.plusDays(1).atStartOfDay.toInstant(timeZone)
                find()
            }
          } else
            endOfData
        }
        find()
      }
    }.asScala

  private def periodSeq(date: LocalDate) =
    weekdays.lift(date.getDayOfWeek)
}

object OldSchedule {
  private val PredictionLimit = 366*24.h

  def empty(timeZone: ZoneId): OldSchedule =
    OldSchedule(timeZone, Map())

  def daily(timeZone: ZoneId, periodSeq: PeriodSeq/*, startOnce: Boolean = false*/) =
    new OldSchedule(timeZone, EveryDay(periodSeq)/*, startOnce*/)

  /**
    * Unlike bare PartialFunction, EveryDay implements toString and equals
    */
  final case class EveryDay(periodSeq: PeriodSeq) extends PartialFunction[DayOfWeek, PeriodSeq] {
    def isDefinedAt(dayOfWeek: DayOfWeek) = true

    override def apply(dayOfWeek: DayOfWeek) = periodSeq

    override def toString = s"EveryDay($periodSeq)"
  }
}
