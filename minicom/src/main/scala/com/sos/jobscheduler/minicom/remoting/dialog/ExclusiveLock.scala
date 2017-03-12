package com.sos.jobscheduler.minicom.remoting.dialog

import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.minicom.remoting.dialog.ExclusiveLock._
import java.util.concurrent.Semaphore

/**
  * @author Joacim Zschimmer
  */
private[dialog] trait ExclusiveLock {
  private val semaphore = new Semaphore(1)

  protected def exclusive[A](body: ⇒ A): A = {
    enterExclusively()
    try body
    finally leaveExclusively()
  }

  protected def enterExclusively(): Unit = {
    if (semaphore.availablePermits == 0) logger.trace(s"Waiting for completion of a concurrent connection dialog")
    if (!semaphore.tryAcquire()) {
      semaphore.acquire()
    }
  }

  protected def leaveExclusively(): Unit = {
    semaphore.release()
  }
}

object ExclusiveLock {
  private val logger = Logger(getClass)
  sealed trait Got
  case object Got extends Got
}
