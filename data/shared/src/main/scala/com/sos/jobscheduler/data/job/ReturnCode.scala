package com.sos.jobscheduler.data.job

import com.sos.jobscheduler.base.generic.GenericInt
import com.sos.jobscheduler.base.process.ProcessSignal

/**
 * @author Joacim Zschimmer
 */
final case class ReturnCode(number: Int) extends GenericInt {

  def isSuccess = number == 0
}

object ReturnCode extends GenericInt.Companion[ReturnCode] {
  def apply(o: Boolean): ReturnCode = if (o) Success else StandardFailure

  def apply(signal: ProcessSignal) = new ReturnCode(128 + signal.value)

  val Success = new ReturnCode(0)
  val StandardFailure = new ReturnCode(1)
}
