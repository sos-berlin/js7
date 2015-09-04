package com.sos.scheduler.engine.data.job

import com.sos.scheduler.engine.base.process.ProcessSignal

/**
 * @author Joacim Zschimmer
 */
final case class ReturnCode(toInt: Int) {
  def isSuccess = toInt == 0

  /**
   * Maps this, as calculated by C++ JobScheduler Engine when process is interrupted, to normal unmodified ReturnCode.
   */
  def normalized = if (toInt >= 0) this else ReturnCode(128 - toInt)
}

object ReturnCode {
  def apply(o: Boolean): ReturnCode = if (o) Success else StandardFailure

  def apply(signal: ProcessSignal) = new ReturnCode(128 + signal.value)

  val Success = new ReturnCode(0)
  val StandardFailure = new ReturnCode(1)
}
