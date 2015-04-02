package com.sos.scheduler.engine.data.job

/**
 * @author Joacim Zschimmer
 */
final case class ResultCode(value: Int) {
  def isSuccess = value == 0
}

object ResultCode {
  def apply(o: Boolean) = new ResultCode(if (o) 0 else 1)
}
