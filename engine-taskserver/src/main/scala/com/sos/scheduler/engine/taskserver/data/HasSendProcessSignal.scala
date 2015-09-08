package com.sos.scheduler.engine.taskserver.data

import com.sos.scheduler.engine.base.process.ProcessSignal

/**
 * @author Joacim Zschimmer
 */
trait HasSendProcessSignal {

  def sendProcessSignal(signal: ProcessSignal): Unit
}
