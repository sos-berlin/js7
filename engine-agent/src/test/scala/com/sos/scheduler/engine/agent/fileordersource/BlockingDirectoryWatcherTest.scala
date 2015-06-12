package com.sos.scheduler.engine.agent.fileordersource

import com.sos.scheduler.engine.common.scalautil.Closers.implicits.{RichClosersAutoCloseable, _}
import com.sos.scheduler.engine.common.scalautil.Closers.withCloser
import com.sos.scheduler.engine.common.scalautil.FileUtils.implicits._
import com.sos.scheduler.engine.common.scalautil.FileUtils.touchAndDeleteWithCloser
import com.sos.scheduler.engine.common.scalautil.Futures.awaitResult
import com.sos.scheduler.engine.common.time.ScalaTime._
import java.nio.file.Files
import java.nio.file.Files.delete
import java.time.Instant.now
import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.junit.JUnitRunner
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

/**
 * @author Joacim Zschimmer
 */
@RunWith(classOf[JUnitRunner])
final class BlockingDirectoryWatcherTest extends FreeSpec {

  "waitForNextChange" in {
    withCloser { implicit closer ⇒
      val directory = Files.createTempDirectory("sos-") withCloser delete
      val matchingPath = directory / "MATCHING-FILE"
      val watcher  = new BlockingDirectoryWatcher(directory, _ == matchingPath).closeWithCloser
      val start = now()
      val until = start + 10.s
      val beforeUntil = start + 7.s

      locally {
        val nonMatchingFuture = Future { watcher.waitForNextChange(until) }
        sleep(1.s)
        assert(!nonMatchingFuture.isCompleted)
        touchAndDeleteWithCloser(directory / "X")
        sleep(1.s)
        val matches = awaitResult(nonMatchingFuture, beforeUntil - now())
        assert(!matches)
      }

      locally {
        val matchingFuture = Future { watcher.waitForNextChange(until) }
        sleep(1.s)
        assert(!matchingFuture.isCompleted)
        touchAndDeleteWithCloser(matchingPath)
        val matches = awaitResult(matchingFuture, beforeUntil - now)
        assert(matches)
      }
    }
  }
}
