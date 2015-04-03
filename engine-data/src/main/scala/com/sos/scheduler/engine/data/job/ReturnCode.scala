package com.sos.scheduler.engine.data.job

/**
 * @author Joacim Zschimmer
 */
final case class ReturnCode(toInt: Int) {
  def isSuccess = toInt == 0
}

object ReturnCode {
  def apply(o: Boolean): ReturnCode = if (o) Success else StandardFailure

  val Success = new ReturnCode(0)
  val StandardFailure = new ReturnCode(1)
}
