package js7.provider

import java.nio.file.Files.{createTempDirectory, delete}
import js7.base.io.file.FileUtils.syntax.*
import js7.base.io.file.FileUtils.{deleteDirectoryRecursively, touchFile}
import js7.base.system.OperatingSystem.isMac
import js7.base.thread.Futures.implicits.*
import js7.base.thread.IOExecutor.Implicits.globalIOX
import js7.base.time.ScalaTime.*
import js7.base.time.WaitForCondition.waitForCondition
import monix.execution.Scheduler.Implicits.traced
import org.scalatest.BeforeAndAfterAll
import org.scalatest.freespec.AnyFreeSpec
import scala.concurrent.duration.*

/**
  * @author Joacim Zschimmer
  */
final class DirectoryWatcherTest extends AnyFreeSpec with BeforeAndAfterAll
{
  private val timeout = if (isMac) 100.ms else 5.minutes
  private lazy val dir = createTempDirectory("DirectoryWatcherTest-")
  private lazy val directoryWatcher = new DirectoryWatcher(dir, timeout)
  private lazy val observable = directoryWatcher.singleUseObservable
  private lazy val observableFuture = observable.map(_ => counter += 1) foreach { _ => }
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
      waitForCondition(99.s, 10.ms)(counter > 0)
      assert(counter > 0)
    }
  } else {
    "Add some files" - {
      for (i <- 1 to 2) s"file #$i" in {
        testUpdate {
          touchFile(dir / i.toString)
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
    sleep(10.ms)
    assert(counter == n)
    body
    waitForCondition(99.s, 10.ms)(counter > n)
    assert(counter > n)
    sleep(10.ms)
  }

  "cancel" in {
    assert(!directoryWatcher.isClosed && !observableFuture.isCompleted)
    observableFuture.cancel()
    observableFuture await 99.s
    assert(directoryWatcher.isClosed)
  }
}
