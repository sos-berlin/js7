package com.sos.jobscheduler.master.oldruntime

import com.google.common.collect.{AbstractIterator ⇒ GuavaIterator}
import com.sos.jobscheduler.common.scalautil.Collections._
import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.master.oldruntime.OldSchedule._
import java.time._
import scala.annotation.tailrec
import scala.collection.JavaConversions._

final case class OldSchedule(
  timeZone: ZoneId,
  weekdays: PartialFunction[DayOfWeek, PeriodSeq],
  startOnce: Boolean)
extends Schedule {

  def firstInstant(from: Instant): Option[Instant] =
    firstInstant(InstantInterval(from, PredictionLimit))

  def firstInstant(instantInterval: InstantInterval): Option[Instant] =
    instants(instantInterval, limit = 1).buffered.headOption

  def instants(instantInterval: InstantInterval, limit: Int = Int.MaxValue): Iterator[Instant] =
    asScalaIterator(
      new GuavaIterator[Instant] {
        private var remaining = limit
        private var from = instantInterval.from

        def computeNext() = {
          @tailrec def find(): Instant = {
            if (remaining > 0 && from < instantInterval.until) {
              val local = LocalDateTime.ofInstant(from, timeZone)
              periodSeq(local.toLocalDate).nextLocalTime(local.toLocalTime) map {
                _.atDate(local.toLocalDate).toInstant(timeZone)
              } match {
                case Some(o) if o < instantInterval.until ⇒
                  remaining -= 1
                  from = o plusNanos 1
                  o
                case Some(_) ⇒
                  endOfData
                case None ⇒
                  from = local.toLocalDate.plusDays(1).atStartOfDay.toInstant(timeZone)
                  find()
              }
            } else
              endOfData
          }
          find()
        }
      })

  private def periodSeq(date: LocalDate) =
    weekdays(date.getDayOfWeek)
}

object OldSchedule {
  private val PredictionLimit = 366*24.h

  def apply(timeZone: ZoneId, periodSeq: PeriodSeq, startOnce: Boolean) =
    new OldSchedule(timeZone, EveryDay(periodSeq), startOnce)

  /**
    * Unlike bare PartialFunction, EveryDay implements toString and equals
    */
  final case class EveryDay(periodSeq: PeriodSeq) extends PartialFunction[DayOfWeek, PeriodSeq] {
    def isDefinedAt(dayOfWeek: DayOfWeek) = true

    override def apply(dayOfWeek: DayOfWeek) = periodSeq

    override def toString = s"EveryDay($periodSeq)"
  }
}
