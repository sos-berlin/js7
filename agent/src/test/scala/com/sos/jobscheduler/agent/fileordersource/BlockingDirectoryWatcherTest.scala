package com.sos.jobscheduler.agent.fileordersource

import com.sos.jobscheduler.common.scalautil.Closer.ops._
import com.sos.jobscheduler.common.scalautil.Closer.withCloser
import com.sos.jobscheduler.common.scalautil.FileUtils.implicits._
import com.sos.jobscheduler.common.scalautil.FileUtils.touchAndDeleteWithCloser
import com.sos.jobscheduler.common.scalautil.Futures.blockingThreadFuture
import com.sos.jobscheduler.common.scalautil.Futures.implicits._
import com.sos.jobscheduler.common.time.ScalaTime._
import java.nio.file.Files.{createTempDirectory, delete}
import java.time.Instant.now
import org.scalatest.FreeSpec
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

/**
 * @author Joacim Zschimmer
 */
final class BlockingDirectoryWatcherTest extends FreeSpec {

  "waitForNextChange" in {
    withCloser { implicit closer =>
      val directory = createTempDirectory("sos-") withCloser delete
      val matchingPath = directory / "MATCHING-FILE"
      val watcher  = new BlockingDirectoryWatcher(directory, _ == matchingPath).closeWithCloser
      val start = now
      val until = start + 5.s + BlockingDirectoryWatcher.PossibleDelay
      val beforeUntil = until - 3.s

      locally {
        val nonMatchingFuture = blockingThreadFuture { watcher.waitForNextChange(until) }
        sleep(1.s)
        assert(!nonMatchingFuture.isCompleted)
        touchAndDeleteWithCloser(directory / "X")
        sleep(1.s)
        val matches = nonMatchingFuture await (beforeUntil - now)
        assert(!matches)
      }

      locally {
        val matchingFuture = Future { watcher.waitForNextChange(until) }
        sleep(1.s)
        assert(!matchingFuture.isCompleted)
        touchAndDeleteWithCloser(matchingPath)
        val matches = matchingFuture await (beforeUntil - now)
        assert(matches)
      }
    }
  }
}
