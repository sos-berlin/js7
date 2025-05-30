package js7.provider

import cats.Show
import cats.effect.IO
import cats.effect.Resource.ExitCase
import fs2.Stream
import java.nio.file.StandardWatchEventKinds.*
import java.nio.file.{Path, WatchEvent}
import js7.base.log.Logger
import js7.base.thread.IOExecutor.env.interruptibleVirtualThread
import js7.base.time.ScalaTime.*
import js7.base.utils.Atomic
import js7.base.utils.AutoClosing.closeOnError
import js7.provider.DirectoryWatcher.*
import scala.concurrent.duration.*
import scala.jdk.CollectionConverters.*

/**
  * @author Joacim Zschimmer
  */
final class DirectoryWatcher(directory: Path, timeout: Duration)
extends AutoCloseable:

  private val watchService = directory.getFileSystem.newWatchService()
  private val closed = Atomic(false)

  closeOnError(watchService):
    // Register early to get the events from now
    directory.register(watchService, ENTRY_CREATE, ENTRY_MODIFY, ENTRY_DELETE)

  def close(): Unit =
    if !closed.getAndSet(true) then
      logger.trace(s"$directory: watchService.close")
      watchService.close()

  def isClosed: Boolean = closed.get()

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
    interruptibleVirtualThread:
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
