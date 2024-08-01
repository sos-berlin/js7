package js7.base.io.file.watch

import cats.data.NonEmptySeq
import cats.effect.IO
import cats.effect.unsafe.IORuntime
import java.nio.file.Files.createDirectory
import java.nio.file.Paths
import java.nio.file.StandardWatchEventKinds.{ENTRY_CREATE, ENTRY_DELETE, ENTRY_MODIFY}
import js7.base.io.file.FileUtils.syntax.*
import js7.base.io.file.FileUtils.temporaryDirectoryResource
import js7.base.io.file.watch.DirectoryEvent.{FileAdded, FileModified}
import js7.base.test.OurAsyncTestSuite
import js7.base.thread.CatsBlocking.syntax.*
import js7.base.thread.Futures.implicits.SuccessFuture
import js7.base.thread.IOExecutor.Implicits.globalIOX
import js7.base.time.ScalaTime.*
import js7.tester.ScalaTestUtils
import js7.tester.ScalaTestUtils.awaitAndAssert
import scala.collection.mutable

final class BasicDirectoryWatchTest extends OurAsyncTestSuite:

  private given IORuntime = ioRuntime

  override protected val withIOExecutor = true

  "step for step" in:
    temporaryDirectoryResource[IO]("DirectoryWatchTest-").use { dir =>
      dir / "ALIEN-1" := ""
      dir / "TEST-1" := ""
      BasicDirectoryWatch
        .resource(
          WatchOptions.forTest(dir, Set(ENTRY_CREATE, ENTRY_DELETE, ENTRY_MODIFY),
            isRelevantFile = _.toString startsWith "TEST-"))
        .use(watcher => IO {
          for i <- 2 to 4 do {
            watcher.streamResource.use(stream => IO.interruptible {
              val observed = stream
                .map(_.filterNot(_.isInstanceOf[FileModified]))  // Windows emits FileModified, too ???
                .take(1).compile.toList.unsafeToFuture()
              // File is detected before stream starts
              //???sleep(100.ms)
              val file = dir / s"TEST-$i"
              Logger.debug(s"touch $file")
              file := ""
              assert(observed.await(99.s) == List(Seq(FileAdded(file.getFileName))))
            }).await(99.s)
          }
          watcher.streamResource.use(stream => IO {
            val observed = stream.take(1).compile.toList.unsafeToFuture()
            dir / "TEST-2" := "MODIFIED"
            assert(observed.await(99.s) == List(Seq(FileModified(Paths.get("TEST-2")))))
          }).await(99.s)
        })
    }

  "BasicDirectoryWatch waits for missing directory" in:
    temporaryDirectoryResource[IO]("DirectoryWatchTest-").use { mainDir =>
      val dir = mainDir / "DIRECTORY"
      // Create directory after start of watching!
      BasicDirectoryWatch
        .resource(
          WatchOptions.forTest(dir, Set(ENTRY_CREATE),
            pollTimeout = 50.ms,
            retryDurations = NonEmptySeq.one(50.ms)))
        .use(watcher => IO.defer:
          val _events = mutable.Buffer.empty[DirectoryEvent]
          watcher.streamResource
            .use(stream => stream
              .foreach(evts => IO:
                _events ++= evts)
              .compile.drain.start
              .flatMap(streaming =>
                IO.interruptible:
                  val file = dir / "1"
                  file := ""
                  awaitAndAssert(_events.lastOption contains FileAdded(file.getFileName))
                .guarantee(streaming.cancel)))
            .both:
              IO:
                createDirectory(dir)
              .delayBy(500.ms) // Delay until stream has started and is waiting for the directory
            .as(succeed))
    }
