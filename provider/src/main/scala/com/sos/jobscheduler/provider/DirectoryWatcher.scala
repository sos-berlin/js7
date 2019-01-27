package com.sos.jobscheduler.provider

import cats.Show
import com.sos.jobscheduler.common.scalautil.AutoClosing.closeOnError
import com.sos.jobscheduler.common.scalautil.IOExecutor.ioFuture
import com.sos.jobscheduler.common.scalautil.{IOExecutor, Logger}
import com.sos.jobscheduler.common.time.ScalaTime.RichConcurrentDuration
import com.sos.jobscheduler.provider.DirectoryWatcher._
import java.nio.file.StandardWatchEventKinds._
import java.nio.file.{ClosedWatchServiceException, Path, WatchEvent}
import java.util.concurrent.TimeUnit.MILLISECONDS
import monix.execution.atomic.AtomicBoolean
import monix.execution.{Ack, Cancelable}
import monix.reactive.Observable
import scala.collection.JavaConverters._
import scala.concurrent.Future
import scala.concurrent.duration._

/**
  * @author Joacim Zschimmer
  */
final class DirectoryWatcher(directory: Path) extends AutoCloseable
{
  private val watchService = directory.getFileSystem.newWatchService()
  private val closed = AtomicBoolean(false)

  closeOnError(watchService) {
    directory.register(watchService, ENTRY_CREATE, ENTRY_MODIFY, ENTRY_DELETE)
  }

  def close() = if (!closed.getAndSet(true)) watchService.close()

  /**
   * Waits until any directory change.
   *
   * @return true, iff A matches `pathMatches` or the event OVERFLOW has occurred or the time is over.
   */
  private def waitForNextChange(timeout: Duration): Boolean = {
    val remainingMillis = timeout.toMillis
    remainingMillis <= 0 || {
      logger.trace(s"$directory poll ${timeout.pretty} ...")
      val watchKey = watchService.poll(remainingMillis, MILLISECONDS)
      logger.trace(s"$directory poll watchKey=$watchKey")
      watchKey != null && {
        try {
          val events = watchKey.pollEvents()
          logger.whenTraceEnabled { events.asScala foreach { o ⇒ logger.trace(s"$directory ${watchEventShow.show(o)}") }}
        }
        finally watchKey.reset()
        true
      }
    }
  }
}

object DirectoryWatcher
{
  private val logger = Logger(getClass)

  def observe(directory: Path, timeout: Duration)(implicit iox: IOExecutor): Observable[Unit] =
    subscriber ⇒ new Cancelable {
      import iox.executionContext
      private val watcher = new DirectoryWatcher(directory)

      def cancel() = {
        logger.trace(s"$directory cancel")
        watcher.close()
      }

      def continue: Future[Unit] =
        ioFuture {
          try {
            watcher.waitForNextChange(timeout)
            subscriber.onNext(()) map {
              case Ack.Continue ⇒ continue
              case Ack.Stop ⇒ watcher.close()
            }
          }
          catch { case e: ClosedWatchServiceException ⇒
            logger.trace(s"$directory subscriber.onComplete due to $e")
            subscriber.onComplete()
          }
        }

      continue
    }

  private implicit val watchEventShow: Show[WatchEvent[_]] = e ⇒
    s"${e.kind.name} ${e.count}× ${e.context}"
}
