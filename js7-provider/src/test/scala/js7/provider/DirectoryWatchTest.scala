package js7.provider

import cats.effect.unsafe.IORuntime
import java.nio.file.Files.{createTempDirectory, delete}
import java.util.concurrent.CancellationException
import js7.base.io.file.FileUtils.syntax.*
import js7.base.io.file.FileUtils.{deleteDirectoryRecursively, touchFile}
import js7.base.monixlike.MonixLikeExtensions.unsafeToCancelableFuture
import js7.base.system.OperatingSystem.isMac
import js7.base.test.OurTestSuite
import js7.base.thread.Futures.implicits.*
import js7.base.thread.IOExecutor.Implicits.globalIOX
import js7.base.time.ScalaTime.*
import js7.tester.ScalaTestUtils.awaitAndAssert
import org.scalatest.BeforeAndAfterAll
import scala.concurrent.duration.*

/**
  * @author Joacim Zschimmer
  */
final class DirectoryWatchTest extends OurTestSuite, BeforeAndAfterAll:

  private given IORuntime = ioRuntime

  private val timeout = if isMac then 100.ms else 5.minutes
  private lazy val dir = createTempDirectory("DirectoryWatchTest-")
  private lazy val directoryWatcher = new DirectoryWatcher(dir, timeout)
  private lazy val stream = directoryWatcher.singleUseStream
  private lazy val streamFuture =
    stream.map(_ => counter += 1).compile.drain.unsafeToCancelableFuture()
  private var counter = 0

  override def beforeAll() =
    super.beforeAll()
    streamFuture

  override def afterAll() =
    try
      directoryWatcher.close()
      deleteDirectoryRecursively(dir)
    finally
      super.afterAll()

  if isMac then
    "Java's WatchService does not work properly under macOS" in:
      // https://bugs.openjdk.java.net/browse/JDK-7133447
      awaitAndAssert(counter > 0)
  else
    "Add some files" - {
      for i <- 1 to 2 do s"file #$i" in:
        testUpdate:
          touchFile(dir / i.toString)
    }

    "Change a file" in:
      testUpdate:
        dir / "1" ++= "X"

    "Delete a file" in:
      testUpdate:
        delete(dir / "1")

  private def testUpdate(body: => Unit): Unit =
    val n = counter
    sleep(10.ms)
    assert(counter == n)
    body
    awaitAndAssert(counter > n)
    sleep(10.ms)

  "cancel" in:
    assert(!directoryWatcher.isClosed && !streamFuture.isCompleted)
    streamFuture.cancelToFuture().await(99.s)
    intercept[CancellationException]:
      streamFuture await 99.s
    assert(directoryWatcher.isClosed)
