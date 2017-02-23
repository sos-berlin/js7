package com.sos.scheduler.engine.common.time.timer.signaling

/**
  * @author Joacim Zschimmer
  */
trait Signaling {

  def signal(): Unit

  /**
    * @return true if signaled, false if timed out
    */
  def awaitMillis(millis: Long): Boolean

  def await(): Unit

  def isSignaled: Boolean
}
