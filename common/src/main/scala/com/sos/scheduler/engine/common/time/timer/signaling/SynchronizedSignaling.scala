package com.sos.scheduler.engine.common.time.timer.signaling

/**
  * @author Joacim Zschimmer
  */
final class SynchronizedSignaling extends Signaling {
  @volatile private var signaled = false

  def signal(): Unit =
    synchronized {
      signaled = true
      notifyAll()
    }

  def awaitMillis(millis: Long): Boolean =
    synchronized {
      if (!signaled && millis > 0) wait(millis)
      val r = signaled
      signaled = false
      r
    }

  def await(): Unit =
    synchronized {
      if (!signaled) wait()
      signaled = false
    }

  def isSignaled = signaled
}
