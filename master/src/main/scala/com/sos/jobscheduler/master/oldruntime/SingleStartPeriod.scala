package com.sos.jobscheduler.master.oldruntime

import com.sos.jobscheduler.base.utils.ScalazStyle.OptionRichBoolean
import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.master.oldruntime.SingleStartPeriod._
import java.time.{Duration, LocalTime}

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
