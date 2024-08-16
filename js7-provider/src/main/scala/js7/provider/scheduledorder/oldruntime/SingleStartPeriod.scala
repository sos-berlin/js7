package js7.provider.scheduledorder.oldruntime

import java.time.{Duration, LocalTime}
import js7.base.time.JavaTime.*
import js7.base.utils.ScalaUtils.syntax.*
import js7.provider.scheduledorder.oldruntime.SingleStartPeriod.*

/**
  * @author Joacim Zschimmer
  */
final case class SingleStartPeriod(at: LocalTime)
extends Period:

  def begin: LocalTime =
    at

  def end: ExtendedLocalTime =
    ExtendedLocalTime.fromLocalTime(at) + Epsilon

  //def repeat = None

  def absoluteRepeatOption: None.type =
    None

  //def startOnce = true

  def nextLocalTime(from: LocalTime): Option[LocalTime] =
    from <= at thenSome at


object SingleStartPeriod:
  private val Epsilon = Duration.ofNanos(1)
