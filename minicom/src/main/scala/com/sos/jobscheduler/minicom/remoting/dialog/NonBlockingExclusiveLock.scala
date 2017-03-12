package com.sos.jobscheduler.minicom.remoting.dialog

import com.sos.jobscheduler.common.scalautil.Futures.implicits.SuccessFuture
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.minicom.remoting.dialog.NonBlockingExclusiveLock._
import java.util.concurrent.ConcurrentLinkedQueue
import java.util.concurrent.atomic.AtomicBoolean
import scala.concurrent.{Future, Promise}

/**
  * @author Joacim Zschimmer
  */
private trait NonBlockingExclusiveLock {
  // Not used, not tested !!!

  private val locked = new AtomicBoolean(false)
  private val queue = new ConcurrentLinkedQueue[Promise[Got]]()

  protected def exclusive[A](body: ⇒ A): A = {
    enterExclusively().awaitInfinite
    try body
    finally leaveExclusively()
  }

  protected def enterExclusively(): Future[Got] = {
    if (locked.get) logger.trace(s"Waiting for completion of a concurrent connection dialog")
    if (locked.compareAndSet(false, true))
      Future.successful(Got)
    else {
      val p = Promise[Got]()
      queue.add(p)
      p.future
    }
  }

  protected def leaveExclusively(): Unit = {
    if (!locked.get) throw new IllegalArgumentException("leaveExclusively without enterExclusively")
    queue.poll() match {
      case null ⇒
      case p ⇒ p.success(Got)
    }
    if (!locked.compareAndSet(true, false)) throw new IllegalArgumentException("leaveExclusively without enterExclusively")
  }
}

private object NonBlockingExclusiveLock {
  private val logger = Logger(getClass)
  sealed trait Got
  case object Got extends Got
}
