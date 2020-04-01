package com.sos.jobscheduler.agent.fileordersource

import com.sos.jobscheduler.base.time.ScalaTime._
import com.sos.jobscheduler.base.utils.Closer.syntax._
import com.sos.jobscheduler.base.utils.Closer.withCloser
import com.sos.jobscheduler.common.scalautil.FileUtils.syntax._
import com.sos.jobscheduler.common.scalautil.FileUtils.touchAndDeleteWithCloser
import com.sos.jobscheduler.common.scalautil.Futures.blockingThreadFuture
import com.sos.jobscheduler.common.scalautil.Futures.implicits._
import java.nio.file.Files.{createTempDirectory, delete}
import org.scalatest.FreeSpec
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.concurrent.duration.Deadline.now

/**
 * @author Joacim Zschimmer
 */
final class BlockingDirectoryWatcherTest extends FreeSpec {

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
