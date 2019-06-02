package com.sos.jobscheduler.provider

import com.google.common.io.MoreFiles.touch
import com.sos.jobscheduler.base.time.ScalaTime._
import com.sos.jobscheduler.common.scalautil.FileUtils.deleteDirectoryRecursively
import com.sos.jobscheduler.common.scalautil.FileUtils.implicits._
import com.sos.jobscheduler.common.scalautil.Futures.implicits._
import com.sos.jobscheduler.common.scalautil.IOExecutor.Implicits.globalIOX
import com.sos.jobscheduler.common.system.OperatingSystem.isMac
import com.sos.jobscheduler.common.time.WaitForCondition.waitForCondition
import java.nio.file.Files.{createTempDirectory, delete}
import monix.execution.Scheduler.Implicits.global
import org.scalatest.{BeforeAndAfterAll, FreeSpec}
import scala.concurrent.duration._

/**
  * @author Joacim Zschimmer
  */
final class DirectoryWatcherTest extends FreeSpec with BeforeAndAfterAll
{
  private val timeout = if (isMac) 100.milliseconds else 5.minutes
  private lazy val dir = createTempDirectory("DirectoryWatcherTest-")
  private lazy val directoryWatcher = new DirectoryWatcher(dir, timeout)
  private lazy val observable = directoryWatcher.singleUseObservable
  private lazy val observableFuture = observable map (_ => counter += 1) foreach { _ => }
  private var counter = 0

  override def beforeAll() = {
    observableFuture
    super.beforeAll()
  }

  override def afterAll() = {
    directoryWatcher.close()
    deleteDirectoryRecursively(dir)
    super.afterAll()
  }

  if (isMac) {
    "Java's WatchService does not work properly under MacOS" in {
      // https://bugs.openjdk.java.net/browse/JDK-7133447
      assert(counter == 0)
      sleep(2 * timeout)
      assert(counter > 0)
    }
  } else {
    "Add some files" - {
      for (i <- 1 to 2) s"file #$i" in {
        testUpdate {
          touch(dir / i.toString)
        }
      }
    }

    "Change a file" in {
      testUpdate {
        dir / "1" ++= "X"
      }
    }

    "Delete a file" in {
      testUpdate {
        delete(dir / "1")
      }
    }
  }

  private def testUpdate(body: => Unit): Unit = {
    val n = counter
    sleep(10.milliseconds)
    assert(counter == n)
    body
    waitForCondition(99.s, 10.ms)(counter > n)
    assert(counter > n)
    sleep(10.milliseconds)
  }

  "cancel" in {
    assert(!directoryWatcher.isClosed && !observableFuture.isCompleted)
    observableFuture.cancel()
    observableFuture await 99.seconds
    assert(directoryWatcher.isClosed)
  }
}
