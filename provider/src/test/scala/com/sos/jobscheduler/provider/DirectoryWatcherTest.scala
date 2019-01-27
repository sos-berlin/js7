package com.sos.jobscheduler.provider

import com.google.common.io.MoreFiles.touch
import com.sos.jobscheduler.base.time.Timestamp.now
import com.sos.jobscheduler.common.scalautil.FileUtils.deleteDirectoryRecursively
import com.sos.jobscheduler.common.scalautil.FileUtils.implicits._
import com.sos.jobscheduler.common.scalautil.Futures.implicits._
import com.sos.jobscheduler.common.scalautil.IOExecutor.Implicits.globalIOX
import com.sos.jobscheduler.common.system.OperatingSystem.isMac
import com.sos.jobscheduler.common.time.ScalaTime._
import java.nio.file.Files.{createTempDirectory, delete}
import java.util.concurrent.CyclicBarrier
import monix.execution.Scheduler.Implicits.global
import org.scalatest.{BeforeAndAfterAll, FreeSpec}
import scala.concurrent.duration._
import scala.concurrent.{Future, blocking}

/**
  * @author Joacim Zschimmer
  */
final class DirectoryWatcherTest extends FreeSpec with BeforeAndAfterAll
{
  private val timeout = 2.seconds
  private lazy val dir = createTempDirectory("DirectoryWatcherTest-")
  private lazy val observable = DirectoryWatcher.observe(dir, timeout = timeout)
  private lazy val observabaleFuture = observable map (_ ⇒ bar()) foreach { _ ⇒ }
  private val barrier = new CyclicBarrier(2)

  private def bar() = barrier.await(99, SECONDS)

  override def beforeAll() = {
    observabaleFuture
    super.beforeAll()
  }
  override def afterAll() = {
    deleteDirectoryRecursively(dir)
    super.afterAll()
  }

  "Add some files" - {
    for (i ← 1 to 2) s"file #$i" in {
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

  private def testUpdate(body: ⇒ Unit): Unit = {
    val delay = 50.milliseconds
    assert(barrier.getNumberWaiting == 0)
    val t = now
    val future = Future {
      blocking {
        sleep(delay)
        body
      }
    }
    bar()
    println(now - t)
    assert(now - t >= delay && (now - t <= timeout / 2 || isMac))
    future await 99.seconds
  }

  "cancel" in {
    assert(!observabaleFuture.isCompleted)
    observabaleFuture.cancel()
    observabaleFuture await 99.seconds
  }
}
