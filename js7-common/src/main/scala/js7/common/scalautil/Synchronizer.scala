package js7.common.scalautil

import java.util.concurrent.locks.ReentrantLock
import js7.base.log.Logger
import js7.base.log.Logger.syntax.*
import js7.base.time.ScalaTime.*
import js7.common.scalautil.Synchronizer.*
import scala.concurrent.blocking
import scala.concurrent.duration.*

/**
  * @author Joacim Zschimmer
  */
final class Synchronizer(what: String):

  private val synchronizeLock = new ReentrantLock  // No fairness

  /** Blocking synchronization with some debug messages logged. */
  def synchronize[A](body: => A): A =
    logger.traceCall(s"synchronize '$what': wait for lock (${synchronizeLock.getQueueLength} in queue)"):
      try
        if !synchronizeLock.tryLock() then
          blocking:
            if !synchronizeLock.tryLock(LogAfter.toMillis, MILLISECONDS) then
              logger.debug(s"🟠 Start synchronize '$what': waiting for lock since ${LogAfter.pretty} (#${synchronizeLock.getQueueLength} in queue)")
              synchronizeLock.lock()
              logger.debug(s"🟢 synchronize '$what': continuing")
        body
      finally
        synchronizeLock.unlock()


object Synchronizer:
  private val LogAfter = 10.ms
  private val logger = Logger[this.type]
