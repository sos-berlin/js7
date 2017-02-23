package com.sos.scheduler.engine.master.oldruntime

import com.sos.scheduler.engine.master.oldruntime.PeriodSeq._
import java.time._
import scala.collection.immutable

abstract sealed case class PeriodSeq(orderedSeq: immutable.Seq[Period]) {

  requireOrderedAndNotOverlapping(orderedSeq)

  def nextLocalTime(from: LocalTime): Option[LocalTime] =
    (orderedSeq flatMap { _.nextLocalTime(from) }).headOption

//  def nextInterval(from: DateTime): Option[Interval] = {
//    val date = from.toDateMidnight
//    val timeOfDay = LocalTime(from.getMillisOfDay)
//    nextPeriod(timeOfDay)  map periodToInterval(date)
//  }

  //def nextPeriod(from: LocalTime): Option[Period] =
  //  orderedSeq find { _ contains from }
}

object PeriodSeq {
  def apply(o: Seq[Period]) =
    new PeriodSeq(o.sorted.to[immutable.Seq]) {}

  private def requireOrderedAndNotOverlapping(orderedPeriods: Iterable[Period]): Unit = {
    if (orderedPeriods.size > 2) {
      for (Seq(a, b) ← orderedPeriods sliding 2) {
        require(a.end <= b.begin, s"Periods overlap: $a and $b")
      }
    }
  }
}
