package js7.common.scalautil

import java.util.concurrent.locks.ReentrantLock
import js7.base.log.Logger
import js7.base.time.ScalaTime.*
import js7.common.scalautil.Synchronizer.*
import scala.concurrent.blocking
import scala.concurrent.duration.*

/**
  * @author Joacim Zschimmer
  */
final class Synchronizer(what: String)
{
  private val synchronizeLock = new ReentrantLock  // No fairness

  /** Blocking synchronization with some debug messages logged. */
  def synchronize[A](body: => A): A =
    try {
      logger.trace(s"Start synchronize '$what': wait for lock (${synchronizeLock.getQueueLength} in queue)")
      if (!synchronizeLock.tryLock()) {
        blocking {
          if (!synchronizeLock.tryLock(LogAfter.toMillis, MILLISECONDS)) {
            logger.debug(s"🔴 Start synchronize '$what': waiting for lock since ${LogAfter.pretty} (#${synchronizeLock.getQueueLength} in queue)")
            synchronizeLock.lock()
            logger.debug(s"synchronize '$what': continuing")
          }
        }
      }
      body
    } finally {
      logger.trace(s"End synchronize '$what': release lock (${synchronizeLock.getQueueLength} in queue)")
      synchronizeLock.unlock()
    }
}

object Synchronizer {
  private val LogAfter = 100.ms
  private val logger = Logger(getClass)
}
