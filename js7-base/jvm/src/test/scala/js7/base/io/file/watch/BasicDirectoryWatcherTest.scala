package js7.base.io.file.watch

import java.nio.file.Files.createDirectory
import java.nio.file.Paths
import java.nio.file.StandardWatchEventKinds.{ENTRY_CREATE, ENTRY_DELETE, ENTRY_MODIFY}
import js7.base.io.file.FileUtils.syntax._
import js7.base.io.file.FileUtils.withTemporaryDirectory
import js7.base.io.file.watch.DirectoryEvent.{FileAdded, FileModified}
import js7.base.log.ScribeUtils.coupleScribeWithSlf4j
import js7.base.thread.IOExecutor.Implicits.globalIOX
import js7.base.thread.MonixBlocking.syntax._
import js7.base.time.ScalaTime._
import js7.base.time.WaitForCondition.waitForCondition
import js7.base.utils.AutoClosing.autoClosing
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import org.scalatest.freespec.AnyFreeSpec
import scala.collection.mutable

final class BasicDirectoryWatcherTest extends AnyFreeSpec
{
  coupleScribeWithSlf4j()

  "step for step" in {
    withTemporaryDirectory("DirectoryWatcherTest-") { dir =>
      dir / "1" := ""
      val watcher = new BasicDirectoryWatcher(
        WatchOptions.forTest(dir, Set(ENTRY_CREATE, ENTRY_DELETE, ENTRY_MODIFY)))
      autoClosing(watcher) { _ =>
        for (i <- 2 to 4) {
          watcher.startObservable.use(observable => Task {
            val observed = observable.take(1).toListL
            // File is detected before observable starts
            val file = dir / i.toString
            file := ""
            assert(observed.await(99.s) == List(Seq(FileAdded(file.getFileName))))
          }).await(99.s)
        }
        watcher.startObservable.use(observable => Task {
          val observed = observable.take(1).toListL
          dir / "2" := "MODIFIED"
          assert(observed.await(99.s) == List(Seq(FileModified(Paths.get("2")))))
        }).await(99.s)
      }
    }
  }

  "BasicDirectoryWatcher waits for missing directory" in {
    withTemporaryDirectory("DirectoryWatcherTest-") { mainDir =>
      val dir = mainDir / "DIRECTORY"
      // Create directory after start of watching!
      val watcher = new BasicDirectoryWatcher(WatchOptions.forTest(dir, Set(ENTRY_CREATE),
        pollTimeout = 50.ms, retryDurations = Seq(50.ms)))
      autoClosing(watcher) { _ =>
        val events = mutable.Buffer.empty[DirectoryEvent]
        val future = watcher.startObservable
          .use(observable => Task {
            val observed = observable foreach { events ++= _ }
            var file = dir / "1"
            file := ""
            waitForCondition(10.s, 10.ms)(events.lastOption contains FileAdded(file.getFileName))
            assert(events.lastOption contains FileAdded(file.getFileName))

            observed.cancel()
          })
        sleep(500.ms)  // Delay until observable has started and is waiting for the directory
        createDirectory(dir)
        future.await(99.s)
      }
    }
  }
}
