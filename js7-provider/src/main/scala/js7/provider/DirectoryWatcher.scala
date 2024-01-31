package js7.provider

import cats.Show
import cats.effect.IO
import cats.effect.kernel.Resource.ExitCase
import java.nio.file.StandardWatchEventKinds.*
import java.nio.file.{ClosedWatchServiceException, Path, WatchEvent}
import js7.base.generic.Completed
import js7.base.log.Logger
import js7.base.thread.IOExecutor
import js7.base.thread.IOExecutor.ioFuture
import js7.base.time.ScalaTime.*
import js7.base.utils.AutoClosing.closeOnError
import js7.base.utils.ScalaUtils.syntax.RichThrowable
import js7.provider.DirectoryWatcher.*
import js7.base.utils.Atomic
import cats.effect.unsafe.IORuntime
import fs2.Stream
import scala.concurrent.Future
import scala.concurrent.duration.*
import scala.jdk.CollectionConverters.*
import scala.util.{Failure, Success}

/**
  * @author Joacim Zschimmer
  */
final class DirectoryWatcher(directory: Path, timeout: Duration)(using IOExecutor, IORuntime)
extends AutoCloseable:

  private val watchService = directory.getFileSystem.newWatchService()
  private val closed = Atomic(false)
  private val subscribed = Atomic(false)

  closeOnError(watchService):
    // Register early to get the events from now
    directory.register(watchService, ENTRY_CREATE, ENTRY_MODIFY, ENTRY_DELETE)

  def close() = if !closed.getAndSet(true) then
    logger.trace(s"$directory: watchService.close")
    watchService.close()

  def isClosed = closed.get()

  /** Stream may only be subscribed to once, because it uses the outer WatchService. */
  def singleUseStream: Stream[IO, Unit] =
    Stream
      .repeatEval:
        waitForNextChange(timeout).void
      .onFinalizeCase:
        case ExitCase.Canceled =>
          IO.blocking:
            logger.trace(s"$directory: cancel")
            DirectoryWatcher.this.close()
        case _ => IO.unit

  /**
   * Waits until any directory change.
   *
   * @return true, iff A matches `pathMatches` or the event OVERFLOW has occurred or the time is over.
   */
  private def waitForNextChange(timeout: Duration): IO[Boolean] =
    IO.interruptible:
      val remainingMillis = timeout.toMillis
      remainingMillis <= 0 || {
        logger.trace(s"$directory: poll ${timeout.pretty} ...")
        val watchKey = watchService.poll(remainingMillis, MILLISECONDS)
        if watchKey == null then
          logger.trace(s"$directory: poll timed out")
          false
        else
          try
            val events = watchKey.pollEvents()
            logger.whenTraceEnabled:
              if events.isEmpty then logger.trace(s"$directory: poll returned no events")
              else events.asScala foreach { o => logger.trace(s"$directory: ${watchEventShow.show(o)}") }
          finally watchKey.reset()
          true
      }


object DirectoryWatcher:
  private val logger = Logger[this.type]

  private implicit val watchEventShow: Show[WatchEvent[?]] = e =>
    s"${e.kind.name} ${e.count}× ${e.context}"
