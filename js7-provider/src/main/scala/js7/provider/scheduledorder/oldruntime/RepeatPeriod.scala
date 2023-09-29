package js7.provider.scheduledorder.oldruntime

import java.time.{Duration, LocalTime}
import js7.base.time.JavaTime.*
import js7.base.utils.ScalaUtils.syntax.*
import js7.provider.scheduledorder.oldruntime.RepeatPeriod.*

final case class RepeatPeriod(
  begin: LocalTime, // = StartOfDay,
  end: ExtendedLocalTime, // = ExtendedLocalTime.EndOfDay,
  //repeat: Option[Duration] = None,
  absoluteRepeat: Duration)
  //startOnce: Boolean = false)
extends Period
{
  require(begin >= StartOfDay, s"RepeatPeriod.begin should be >= ${ExtendedLocalTime.StartOfDay}: $begin")
  require(end <= ExtendedLocalTime.EndOfDay, s"RepeatPeriod.end should be <= ${ExtendedLocalTime.EndOfDay}: $end")
  require(begin.toNanoOfDay <= end.toNanoOfDay, s"RepeatPeriod.begin should not be after end: begin=$begin end=$end")
  //for (o <- repeat ++ absoluteRepeat) require(o.toMillis > 0, s"repeat should be positive")
  //require(repeat.isEmpty || absoluteRepeat.isEmpty, s"Only one of attributes repeat and absolute_repeat is allowed")

  override def absoluteRepeatOption = Some(absoluteRepeat)

  def nextLocalTime(t: LocalTime): Option[LocalTime] =
    if t < begin then
      Some(begin)
    else {
      val last = (t.toNanoOfDay + absoluteRepeat.toNanos - begin.toNanoOfDay) / absoluteRepeat.toNanos
      val next = begin.toNanoOfDay + last * absoluteRepeat.toNanos
      next < end.toNanoOfDay option
        LocalTime.ofNanoOfDay(next)
    }

  //def contains(o: LocalTime) =
  //  begin.toNanoOfDay >= o.toNanoOfDay && o.toNanoOfDay < end.toNanoOfDay

  //def toInstantInterval(date: LocalDate, timeZone: ZoneId) =
  //  InstantInterval(begin.atDate(date).toInstant(timeZone), end.atDate(date).toInstant(timeZone))
}

object RepeatPeriod {
  val StartOfDay = LocalTime.of(0, 0)

  def wholeDay(absoluteRepeat: Duration): RepeatPeriod =
    RepeatPeriod(LocalTime.MIN, ExtendedLocalTime.EndOfDay, absoluteRepeat)
}
