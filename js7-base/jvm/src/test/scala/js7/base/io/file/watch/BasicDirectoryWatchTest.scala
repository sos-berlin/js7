package js7.base.io.file.watch

import cats.data.NonEmptySeq
import java.nio.file.Files.createDirectory
import java.nio.file.Paths
import java.nio.file.StandardWatchEventKinds.{ENTRY_CREATE, ENTRY_DELETE, ENTRY_MODIFY}
import js7.base.io.file.FileUtils.syntax.*
import js7.base.io.file.FileUtils.withTemporaryDirectory
import js7.base.io.file.watch.DirectoryEvent.{FileAdded, FileModified}
import js7.base.test.OurTestSuite
import js7.base.thread.IOExecutor.Implicits.globalIOX
import js7.base.thread.MonixBlocking.syntax.*
import js7.base.time.ScalaTime.*
import js7.base.time.WaitForCondition.waitForCondition
import monix.eval.Task
import monix.execution.Scheduler.Implicits.traced
import scala.collection.mutable

final class BasicDirectoryWatchTest extends OurTestSuite:
  "step for step" in:
    withTemporaryDirectory("DirectoryWatchTest-") { dir =>
      dir / "ALIEN-1" := ""
      dir / "TEST-1" := ""
      BasicDirectoryWatch
        .resource(
          WatchOptions.forTest(dir, Set(ENTRY_CREATE, ENTRY_DELETE, ENTRY_MODIFY),
            isRelevantFile = _.toString startsWith "TEST-"))
        .use(watcher => Task {
          for i <- 2 to 4 do {
            watcher.observableResource.use(observable => Task {
              val observed = observable
                .map(_.filterNot(_.isInstanceOf[FileModified]))  // Windows emits FileModified, too ???
                .take(1).toListL
              // File is detected before observable starts
              val file = dir / s"TEST-$i"
              file := ""
              assert(observed.await(99.s) == List(Seq(FileAdded(file.getFileName))))
            }).await(99.s)
          }
          watcher.observableResource.use(observable => Task {
            val observed = observable.take(1).toListL
            dir / "TEST-2" := "MODIFIED"
            assert(observed.await(99.s) == List(Seq(FileModified(Paths.get("TEST-2")))))
          }).await(99.s)
        })
        .await(99.s)
    }

  "BasicDirectoryWatch waits for missing directory" in:
    withTemporaryDirectory("DirectoryWatchTest-") { mainDir =>
      val dir = mainDir / "DIRECTORY"
      // Create directory after start of watching!
      BasicDirectoryWatch
        .resource(
          WatchOptions.forTest(dir, Set(ENTRY_CREATE),
          pollTimeout = 50.ms, retryDurations = NonEmptySeq.one(50.ms)))
        .use(watcher => Task {
          val events = mutable.Buffer.empty[DirectoryEvent]
          val future = watcher.observableResource
            .use(observable => Task {
              val observed = observable foreach { events ++= _ }
              val file = dir / "1"
              file := ""
              waitForCondition(10.s, 10.ms)(events.lastOption contains FileAdded(file.getFileName))
              assert(events.lastOption contains FileAdded(file.getFileName))

              observed.cancel()
            })
          sleep(500.ms)  // Delay until observable has started and is waiting for the directory
          createDirectory(dir)
          future.await(99.s)
        })
        .await(99.s)
    }
