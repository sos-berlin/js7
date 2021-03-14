package js7.base.io.file.watch

import java.nio.file.Paths
import java.nio.file.StandardWatchEventKinds.{ENTRY_CREATE, ENTRY_DELETE, ENTRY_MODIFY}
import js7.base.io.file.FileUtils.syntax._
import js7.base.io.file.FileUtils.withTemporaryDirectory
import js7.base.io.file.watch.DirectoryEvent.{FileAdded, FileModified}
import js7.base.log.ScribeUtils.coupleScribeWithSlf4j
import js7.base.thread.IOExecutor.Implicits.globalIOX
import js7.base.thread.MonixBlocking.syntax.RichTask
import js7.base.time.ScalaTime._
import js7.base.utils.AutoClosing.autoClosing
import monix.execution.Scheduler.Implicits.global
import org.scalatest.freespec.AnyFreeSpec

final class BasicDirectoryWatcherTest extends AnyFreeSpec
{
  coupleScribeWithSlf4j()

  "step for step" in {
    withTemporaryDirectory("DirectoryWatcherTest-") { dir =>
      dir / "1" := ""
      val watcher = new BasicDirectoryWatcher(
        WatchOptions(dir, Set(ENTRY_CREATE, ENTRY_DELETE, ENTRY_MODIFY), pollDuration = 99.s))
      autoClosing(watcher) { _ =>
        for (i <- 2 to 4) {
          // File is detected before observable starts
          val file = dir / i.toString
          file := ""
          watcher.observe.take(1).toListL.await(99.s) == List(FileAdded(file.getFileName))
        }
        dir / "2" := "MODIFIED"
        watcher.observe.take(1).toListL.await(99.s) == List(FileModified(Paths.get("2")))
      }
    }
  }
}
