package js7.base.io.file.watch

import cats.data.NonEmptySeq
import java.nio.file.Files.createDirectory
import java.nio.file.Paths
import java.nio.file.StandardWatchEventKinds.{ENTRY_CREATE, ENTRY_DELETE, ENTRY_MODIFY}
import js7.base.io.file.FileUtils.syntax.*
import cats.effect.IO
import js7.base.io.file.FileUtils.withTemporaryDirectory
import js7.base.io.file.watch.DirectoryEvent.{FileAdded, FileModified}
import js7.base.test.OurTestSuite
import js7.base.thread.IOExecutor.Implicits.globalIOX
import js7.base.thread.CatsBlocking.syntax.*
import js7.base.time.ScalaTime.*
import js7.tester.ScalaTestUtils.awaitAndAssert
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
        .use(watcher => IO {
          for i <- 2 to 4 do {
            watcher.streamResource.use(stream => IO {
              val observed = stream
                .map(_.filterNot(_.isInstanceOf[FileModified]))  // Windows emits FileModified, too ???
                .take(1).toListL
              // File is detected before stream starts
              val file = dir / s"TEST-$i"
              file := ""
              assert(observed.await(99.s) == List(Seq(FileAdded(file.getFileName))))
            }).await(99.s)
          }
          watcher.streamResource.use(stream => IO {
            val observed = stream.take(1).toListL
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
        .use(watcher => IO {
          val events = mutable.Buffer.empty[DirectoryEvent]
          val future = watcher.streamResource
            .use(stream => IO {
              val observed = stream foreach { events ++= _ }
              val file = dir / "1"
              file := ""
              awaitAndAssert(events.lastOption contains FileAdded(file.getFileName))

              observed.cancel()
            })
          sleep(500.ms)  // Delay until stream has started and is waiting for the directory
          createDirectory(dir)
          future.await(99.s)
        })
        .await(99.s)
    }
