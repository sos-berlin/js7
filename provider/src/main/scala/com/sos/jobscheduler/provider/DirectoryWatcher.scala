package com.sos.jobscheduler.provider

import cats.Show
import com.sos.jobscheduler.base.generic.Completed
import com.sos.jobscheduler.common.scalautil.AutoClosing.closeOnError
import com.sos.jobscheduler.common.scalautil.IOExecutor.ioFuture
import com.sos.jobscheduler.common.scalautil.{IOExecutor, Logger}
import com.sos.jobscheduler.common.time.ScalaTime.RichConcurrentDuration
import com.sos.jobscheduler.provider.DirectoryWatcher._
import java.nio.file.StandardWatchEventKinds._
import java.nio.file.{ClosedWatchServiceException, Path, WatchEvent}
import java.util.concurrent.TimeUnit.MILLISECONDS
import monix.execution.atomic.AtomicBoolean
import monix.execution.{Ack, Cancelable, Scheduler}
import monix.reactive.Observable
import scala.collection.JavaConverters._
import scala.concurrent.Future
import scala.concurrent.duration._
import scala.util.{Failure, Success}

/**
  * @author Joacim Zschimmer
  */
final class DirectoryWatcher(directory: Path, timeout: Duration)(implicit iox: IOExecutor, s: Scheduler)
extends AutoCloseable
{
  private val watchService = directory.getFileSystem.newWatchService()
  private val closed = AtomicBoolean(false)
  private val subscribed = AtomicBoolean(false)

  closeOnError(watchService) {
    // Register early to get the events from now
    directory.register(watchService, ENTRY_CREATE, ENTRY_MODIFY, ENTRY_DELETE)
  }

  def close() = if (!closed.getAndSet(true)) {
    logger.trace(s"$directory: watchService.close")
    watchService.close()
  }

  def isClosed = closed.get

  /** Observable may only be subscribed to once, because it uses the outer WatchService. */
  def singleUseObservable: Observable[Unit] =
    subscriber => {
      if (subscribed.getAndSet(true)) sys.error("DirectoryWatcher#singleUseObservable is subscribable only once")
      new Cancelable {
        def cancel() = {
          logger.trace(s"$directory: cancel")
          DirectoryWatcher.this.close()
        }

        def continue(): Future[Completed] =
          ioFuture { waitForNextChange(timeout) }
            .flatMap(_ => subscriber.onNext(())
            .flatMap {
              case Ack.Continue => continue()
              case Ack.Stop => Future.successful(Completed)
            })

        continue() onComplete {
          case Success(Completed) =>
            logger.trace(s"$directory: completed")
          case Failure(e: ClosedWatchServiceException) =>
            logger.trace(s"$directory: subscriber.onComplete due to $e")
            subscriber.onComplete()
          case Failure(t) =>
            logger.trace(s"$directory: $t")
            subscriber.onError(t)
        }
      }
    }

  /**
   * Waits until any directory change.
   *
   * @return true, iff A matches `pathMatches` or the event OVERFLOW has occurred or the time is over.
   */
  private def waitForNextChange(timeout: Duration): Boolean = {
    val remainingMillis = timeout.toMillis
    remainingMillis <= 0 || {
      logger.trace(s"$directory: poll ${timeout.pretty} ...")
      val watchKey = watchService.poll(remainingMillis, MILLISECONDS)
      logger.trace(s"$directory: poll watchKey=$watchKey")
      watchKey != null && {
        try {
          val events = watchKey.pollEvents()
          logger.whenTraceEnabled { events.asScala foreach { o => logger.trace(s"$directory: ${watchEventShow.show(o)}") }}
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

  private implicit val watchEventShow: Show[WatchEvent[_]] = e =>
    s"${e.kind.name} ${e.count}× ${e.context}"
}
