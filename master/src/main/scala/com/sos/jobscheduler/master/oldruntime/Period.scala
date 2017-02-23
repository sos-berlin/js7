package com.sos.scheduler.engine.master.oldruntime

import com.sos.scheduler.engine.common.time.ScalaTime._
import com.sos.scheduler.engine.master.oldruntime.Period._
import java.time._

final case class Period(
  begin: LocalTime = StartOfDay,
  end: ExtendedLocalTime = ExtendedLocalTime.EndOfDay,
  repeat: Option[Duration] = None,
  absoluteRepeat: Option[Duration] = None,
  startOnce: Boolean = false)
extends Ordered[Period]
{
  require(begin >= StartOfDay, s"Period.begin should be >= ${ExtendedLocalTime.StartOfDay}: $begin")
  require(end <= ExtendedLocalTime.EndOfDay, s"Period.end should be <= ${Default.end}: $end")
  require(begin.toNanoOfDay <= end.toNanoOfDay, s"Period.begin should not be after end: begin=$begin end=$end")
  for (o ← repeat ++ absoluteRepeat) require(o.toMillis > 0, s"repeat should be positive")
  require(repeat.isEmpty || absoluteRepeat.isEmpty, s"Only one of attributes repeat and absolute_repeat is possible")

  def compare(o: Period) = begin compareTo o.begin

  def nextLocalTime(t: LocalTime): Option[LocalTime] =
    if (hasStart && t < begin)
      Some(begin)
    else
      for (a ← absoluteRepeat;
           last = (t.toNanoOfDay + a.toNanos - begin.toNanoOfDay) / a.toNanos;
           next = begin.toNanoOfDay + last * a.toNanos if next < end.toNanoOfDay)
      yield
        LocalTime.ofNanoOfDay(next)

  def hasStart =
    startOnce || repeat.nonEmpty || absoluteRepeat.nonEmpty

  def contains(o: LocalTime) =
    begin.toNanoOfDay >= o.toNanoOfDay && o.toNanoOfDay < end.toNanoOfDay

  def toInstantInterval(date: LocalDate, timeZone: ZoneId) =
    InstantInterval(begin.atDate(date).toInstant(timeZone), end.atDate(date).toInstant(timeZone))
}

object Period {
  val StartOfDay = LocalTime.of(0, 0)
  val Default = Period()
}
