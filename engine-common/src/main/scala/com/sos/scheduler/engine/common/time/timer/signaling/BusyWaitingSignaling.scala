package com.sos.scheduler.engine.common.time.timer.signaling

import java.lang.System.currentTimeMillis
import java.util.concurrent.atomic.AtomicBoolean
import org.jetbrains.annotations.TestOnly

/**
  * Very fast, but ...
  * @author Joacim Zschimmer
  */
@TestOnly
private[timer] final class BusyWaitingSignaling extends Signaling {
  private val signaled = new AtomicBoolean

  def signal() = signaled.set(true)

  def awaitMillis(millis: Long): Boolean = {
    val until = currentTimeMillis + millis
    do if (signaled.compareAndSet(true, false)) return true
    while (currentTimeMillis < until)
    false
  }

  def await() = while (signaled.compareAndSet(true, false)) {}

  def isSignaled = signaled.get
}
