package com.sos.scheduler.engine.data.job

import com.sos.scheduler.engine.base.generic.GenericInt
import com.sos.scheduler.engine.base.process.ProcessSignal

/**
 * @author Joacim Zschimmer
 */
final case class ReturnCode(number: Int) extends GenericInt {

  def isSuccess = number == 0

  /**
   * Maps this, as calculated by C++ JobScheduler Engine when process is interrupted, to normal unmodified ReturnCode.
   */
  def normalized = if (number >= 0) this else ReturnCode(128 - number)
}

object ReturnCode extends GenericInt.Companion[ReturnCode] {
  def apply(o: Boolean): ReturnCode = if (o) Success else StandardFailure

  def apply(signal: ProcessSignal) = new ReturnCode(128 + signal.value)

  val Success = new ReturnCode(0)
  val StandardFailure = new ReturnCode(1)
}
