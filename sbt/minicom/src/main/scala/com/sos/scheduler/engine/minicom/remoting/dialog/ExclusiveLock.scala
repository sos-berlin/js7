package com.sos.scheduler.engine.minicom.remoting.dialog

import com.sos.scheduler.engine.common.scalautil.Logger
import com.sos.scheduler.engine.minicom.remoting.dialog.ExclusiveLock._
import java.util.concurrent.locks.ReentrantLock

/**
  * @author Joacim Zschimmer
  */
private[dialog] trait ExclusiveLock {
  private val lock = new ReentrantLock

  protected def exclusive[A](body: ⇒ A): A = {
    if (lock.isLocked) logger.trace(s"Waiting for completion of a concurrent connection dialog")
    if (lock.getHoldCount != 0) throw new IllegalStateException("Recursive call")
    lock.lock()
    try body
    finally lock.unlock()
  }
}

object ExclusiveLock {
  private val logger = Logger(getClass)
}
