package js7.provider.scheduledorder.oldruntime

import java.time.{Duration, LocalTime}
import js7.base.utils.ScalazStyle.OptionRichBoolean
import js7.common.time.JavaTime._
import js7.provider.scheduledorder.oldruntime.SingleStartPeriod._

/**
  * @author Joacim Zschimmer
  */
final case class SingleStartPeriod(at: LocalTime)
extends Period {

  def begin = at

  def end = ExtendedLocalTime.fromLocalTime(at) + Epsilon

  //def repeat = None

  def absoluteRepeatOption = None

  //def startOnce = true

  def nextLocalTime(from: LocalTime) =
    from <= at option at
}

object SingleStartPeriod {
  private val Epsilon = Duration.ofNanos(1)
}
