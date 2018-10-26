package com.sos.jobscheduler.common.scalautil

import com.sos.jobscheduler.common.scalautil.Synchronizer._
import com.sos.jobscheduler.common.time.ScalaTime.RichConcurrentDuration
import java.util.concurrent.TimeUnit.MILLISECONDS
import java.util.concurrent.locks.ReentrantLock
import scala.concurrent.blocking
import scala.concurrent.duration._

/**
  * @author Joacim Zschimmer
  */
final class Synchronizer(what: String)
{
  private val synchronizeLock = new ReentrantLock

  /** Blocking synchronization with some debug messages logged. */
  def synchronize[A](body: ⇒ A): A =
    blocking {
      try {
        ignoreException { logger.trace(s"Start synchronize '$what': wait for lock (${synchronizeLock.getQueueLength} in queue)") }
        if (!synchronizeLock.tryLock(DebugDuration.toMillis, MILLISECONDS)) {
          ignoreException { logger.debug(s"Start synchronize '$what': waiting for lock since ${DebugDuration.pretty} (#${synchronizeLock.getQueueLength} in queue)") }
          synchronizeLock.lock()
          ignoreException { logger.debug(s"synchronize '$what': continuing") }
        }
        body
      } finally {
        ignoreException { logger.trace(s"End synchronize '$what': release lock (${synchronizeLock.getQueueLength} in queue)") }
        synchronizeLock.unlock()
      }
    }
}

object Synchronizer {
  private val DebugDuration = 100.milliseconds
  private val logger = Logger(getClass)

  private def ignoreException(body: ⇒ Unit) =
    try body
    catch {
      case _: Throwable ⇒
    }
}
