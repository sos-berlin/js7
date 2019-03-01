package com.sos.jobscheduler.provider.scheduledorder.oldruntime

import com.sos.jobscheduler.provider.scheduledorder.oldruntime.PeriodSeq._
import java.time._
import scala.collection.immutable
import scala.language.implicitConversions

abstract sealed case class PeriodSeq(orderedSeq: immutable.Seq[Period]) {

  requireOrderedAndNotOverlapping(orderedSeq)

  def nextLocalTime(from: LocalTime): Option[LocalTime] =
    (orderedSeq flatMap { _.nextLocalTime(from) }).headOption

//  def nextInterval(from: DateTime): Option[Interval] = {
//    val date = from.toDateMidnight
//    val timeOfDay = LocalTime(from.getMillisOfDay)
//    nextPeriod(timeOfDay)  map periodToInterval(date)
//  }

  //def nextPeriod(from: LocalTime): Option[RepeatPeriod] =
  //  orderedSeq find { _ contains from }
}

object PeriodSeq {
  val Empty = PeriodSeq(Nil)

  def apply(o: Seq[Period]) =
    new PeriodSeq(o.sorted.to[immutable.Seq]) {}

  implicit def fromPeriod(period: Period): PeriodSeq =
    PeriodSeq(List(period))

  private def requireOrderedAndNotOverlapping(orderedPeriods: Iterable[Period]): Unit = {
    if (orderedPeriods.size > 2) {
      for (Seq(a, b) <- orderedPeriods sliding 2) {
        require(a.end <= b.begin, s"Periods overlap: $a and $b")
      }
    }
  }
}
