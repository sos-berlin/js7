package js7.common.scalautil

import java.util.concurrent.locks.ReentrantLock
import js7.base.time.ScalaTime._
import js7.common.scalautil.Synchronizer._
import scala.concurrent.blocking
import scala.concurrent.duration._

/**
  * @author Joacim Zschimmer
  */
final class Synchronizer(what: String)
{
  private val synchronizeLock = new ReentrantLock  // No fairness

  /** Blocking synchronization with some debug messages logged. */
  def synchronize[A](body: => A): A =
    try {
      ignoreException { logger.trace(s"Start synchronize '$what': wait for lock (${synchronizeLock.getQueueLength} in queue)") }
      if (!synchronizeLock.tryLock()) {
        blocking {
          if (!synchronizeLock.tryLock(LogAfter.toMillis, MILLISECONDS)) {
            ignoreException { logger.debug(s"Start synchronize '$what': waiting for lock since ${LogAfter.pretty} (#${synchronizeLock.getQueueLength} in queue)") }
            synchronizeLock.lock()
            ignoreException { logger.debug(s"synchronize '$what': continuing") }
          }
        }
      }
      body
    } finally {
      ignoreException { logger.trace(s"End synchronize '$what': release lock (${synchronizeLock.getQueueLength} in queue)") }
      synchronizeLock.unlock()
    }
}

object Synchronizer {
  private val LogAfter = 100.milliseconds
  private val logger = Logger(getClass)

  private def ignoreException(body: => Unit) =
    try body
    catch {
      case _: Throwable =>
    }
}
