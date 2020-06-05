package js7.agent.fileordersource

import js7.base.time.ScalaTime._
import js7.base.utils.Closer.syntax._
import js7.base.utils.Closer.withCloser
import js7.common.scalautil.FileUtils.syntax._
import js7.common.scalautil.FileUtils.touchAndDeleteWithCloser
import js7.common.scalautil.Futures.blockingThreadFuture
import js7.common.scalautil.Futures.implicits._
import java.nio.file.Files.{createTempDirectory, delete}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.concurrent.duration.Deadline.now
import org.scalatest.freespec.AnyFreeSpec

/**
 * @author Joacim Zschimmer
 */
final class BlockingDirectoryWatcherTest extends AnyFreeSpec {

  "waitForNextChange" in {
    withCloser { implicit closer =>
      val directory = createTempDirectory("sos-") withCloser delete
      val matchingPath = directory / "MATCHING-FILE"
      val watcher  = new BlockingDirectoryWatcher(directory, _ == matchingPath).closeWithCloser
      val deadline = now + 5.s + BlockingDirectoryWatcher.PossibleDelay
      val beforeUntil = deadline - 3.s

      locally {
        val nonMatchingFuture = blockingThreadFuture { watcher.waitForNextChange(deadline) }
        sleep(1.s)
        assert(!nonMatchingFuture.isCompleted)
        touchAndDeleteWithCloser(directory / "X")
        sleep(1.s)
        val matches = nonMatchingFuture await beforeUntil.timeLeft
        assert(!matches)
      }

      locally {
        val matchingFuture = Future { watcher.waitForNextChange(deadline) }
        sleep(1.s)
        assert(!matchingFuture.isCompleted)
        touchAndDeleteWithCloser(matchingPath)
        val matches = matchingFuture await beforeUntil.timeLeft
        assert(matches)
      }
    }
  }
}
