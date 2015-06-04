package com.sos.scheduler.engine.taskserver

import com.sos.scheduler.engine.base.process.ProcessSignal

/**
 * @author Joacim Zschimmer
 */
trait HasSendProcessSignal {

  def sendProcessSignal(signal: ProcessSignal): Unit
}
