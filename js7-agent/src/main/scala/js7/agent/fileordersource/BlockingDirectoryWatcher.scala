package js7.agent.fileordersource

import java.nio.file.StandardWatchEventKinds._
import java.nio.file.{FileSystems, Path, WatchEvent}
import js7.agent.fileordersource.BlockingDirectoryWatcher._
import js7.base.time.ScalaTime._
import js7.base.utils.Closer.syntax.RichClosersAutoCloseable
import js7.base.utils.HasCloser
import js7.common.scalautil.Logger
import js7.common.system.OperatingSystem.isMac
import scala.concurrent._
import scala.concurrent.duration._
import scala.jdk.CollectionConverters._

/**
 * @author Joacim Zschimmer
 *
 * @param pathMatches Predicate for a `Path` resolved against `directory`
 */
private[fileordersource] final class BlockingDirectoryWatcher(directory: Path, pathMatches: Path => Boolean) extends HasCloser {

  private val watchService = FileSystems.getDefault.newWatchService().closeWithCloser

  directory.register(watchService, ENTRY_CREATE)

  def waitForMatchingDirectoryChange(until: Deadline): Unit = while (!waitForNextChange(until)) {}

  /**
   * Waits until any directory change.
   *
   * @return true, iff Path matches `pathMatches` or the event OVERFLOW has occurred or the time is over.
   */
  def waitForNextChange(until: Deadline): Boolean = {
    val remaining = until.timeLeft
    remaining <= Duration.Zero || {
      lazy val logPrefix = s"Watching directory $directory"
      logger.trace(s"$logPrefix for ${remaining.pretty} ...")
      val watchKey = blocking {
        watchService.poll(remaining.length, remaining.unit)
      }
      if (watchKey == null) {
        logger.trace(s"$logPrefix, expired")
        false
      } else
        try
          watchKey.pollEvents().asInstanceOf[java.util.List[WatchEvent[Path]]].asScala exists { event =>
            logger.trace(s"$logPrefix, event ${event.kind} ${event.context}")
            event.kind == OVERFLOW || pathMatches(directory resolve event.context)
          }
        finally watchKey.reset()
    }
  }
}

object BlockingDirectoryWatcher {
  val PossibleDelay: FiniteDuration = if (isMac) 30.s else 5.s  // Slow for macOS. See https://bugs.openjdk.java.net/browse/JDK-7133447
  private val logger = Logger(getClass)
}
