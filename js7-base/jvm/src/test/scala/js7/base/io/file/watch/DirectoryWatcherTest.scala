package js7.base.io.file.watch

import com.google.common.io.MoreFiles.touch
import java.nio.file.Files.{createDirectory, delete}
import java.nio.file.Paths
import java.nio.file.StandardWatchEventKinds.{ENTRY_CREATE, ENTRY_DELETE}
import js7.base.io.file.FileUtils.syntax._
import js7.base.io.file.FileUtils.withTemporaryDirectory
import js7.base.io.file.watch.DirectoryEvent.{FileAdded, FileDeleted}
import js7.base.io.file.watch.DirectoryState.Entry
import js7.base.log.ScribeUtils.coupleScribeWithSlf4j
import js7.base.thread.Futures.implicits.SuccessFuture
import js7.base.thread.IOExecutor.Implicits.globalIOX
import js7.base.thread.MonixBlocking.syntax.RichTask
import js7.base.time.ScalaTime._
import js7.base.time.WaitForCondition.waitForCondition
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import monix.reactive.Observable
import monix.reactive.subjects.PublishSubject
import org.scalatest.freespec.AnyFreeSpec
import scala.collection.mutable
import scala.concurrent.Promise
import scala.math.Ordering.Int

final class DirectoryWatcherTest extends AnyFreeSpec
{
  coupleScribeWithSlf4j()

  "readDirectory, readDirectoryAsEvents" in {
    withTemporaryDirectory("DirectoryWatcherTest-") { dir =>
      touch(dir / "1")
      touch(dir / ".hidden")
      touch(dir / "2")
      val state = DirectoryState.readDirectory(dir)
      assert(state == DirectoryState(Map(
        Paths.get("1") -> DirectoryState.Entry(Paths.get("1")),
        Paths.get("2") -> DirectoryState.Entry(Paths.get("2")))))

      touch(dir / "A")
      touch(dir / "1")

      assert(state.diffTo(DirectoryState.readDirectory(dir)).toSet ==
        Set(FileAdded(Paths.get("A"))))
    }
  }

  "readDirectoryAndObserve" in {
    val directoryEvents = PublishSubject[Seq[DirectoryEvent]]()
    @volatile var files = Seq("0")

    def addFile(name: String) = {
      files :+= name
      directoryEvents.onNext(Seq(FileAdded(Paths.get(name)))) await 99.s
    }

    def toDirectoryState(names: String*) =
      DirectoryState.fromIterable(names.map(Paths.get(_)).map(Entry(_)))

    def readDirectory() =
      toDirectoryState(files: _*)

    val watcher = new DirectoryWatcher(Task(readDirectory()), directoryEvents)

    def observe(state: DirectoryState, n: Int) =
      watcher.readDirectoryAndObserve(state)
        .take(n).toListL.await(99.s)

    var state = readDirectory()
    addFile("1")
    assert(observe(state, 1) == Seq(
      Seq(FileAdded(Paths.get("1"))) -> toDirectoryState("0", "1")))

    // The duplicate event will be ignored
    directoryEvents.onNext(Seq(FileAdded(Paths.get("1")))) await 99.s

    state = readDirectory()
    addFile("2")
    assert(observe(state, 1) == Seq(
      Seq(FileAdded(Paths.get("2"))) -> toDirectoryState("0", "1", "2")))
  }

  "observe" in {
    withTemporaryDirectory("DirectoryWatcherTest-") { dir =>
      val options = WatchOptions.forTest(dir, Set(ENTRY_CREATE, ENTRY_DELETE))
      val buffer = mutable.Buffer[Set[DirectoryEvent]]()
      val subscribed = Promise[Unit]()
      val stop = PublishSubject[Unit]()
      val whenObserved = DirectoryWatcher.observable(DirectoryState.empty, options)
        .doOnSubscribe(Task(subscribed.success(())))
        .takeUntil(stop)
        .foreach(buffer += _.toSet)
      subscribed.future await 99.s
      assert(buffer.isEmpty)
      touch(dir / "1")
      waitForCondition(11.s, 10.ms)(buffer == Seq(Set(FileAdded(Paths.get("1")))))
      assert(buffer == Seq(Set(FileAdded(Paths.get("1")))))
      stop.onComplete()
      whenObserved.await(99.s)
    }
  }

  "Observing a deleted and recreated directory" in {
    withTemporaryDirectory("DirectoryWatcherTest-") { mainDir =>
      val dir = mainDir / "DIRECTORY"
      val options = WatchOptions.forTest(dir, Set(ENTRY_CREATE, ENTRY_DELETE), pollTimeout = 100.ms)
      val buffer = mutable.Buffer[DirectoryEvent]()
      val subscribed = Promise[Unit]()
      val stop = PublishSubject[Unit]()
      val whenObserved = DirectoryWatcher.observable(DirectoryState.empty, options)
        .doOnSubscribe(Task(subscribed.success(())))
        .takeUntil(stop)
        .foreach(buffer ++= _)
      sleep(500.ms)  // Delay directory creation
      createDirectory(dir)
      subscribed.future await 99.s
      assert(buffer.isEmpty)
      touch(dir / "1")
      waitForCondition(11.s, 10.ms)(buffer contains FileAdded(Paths.get("1")))
      assert(buffer contains FileAdded(Paths.get("1")))
      delete(dir / "1")

      delete(dir)
      sleep(300.ms)
      createDirectory(dir)
      touch(dir / "2")
      waitForCondition(11.s, 10.ms)(buffer.contains(FileDeleted(Paths.get("1"))) && buffer.contains(FileAdded(Paths.get("2"))))
      assert(buffer.contains(FileDeleted(Paths.get("1"))) && buffer.contains(FileAdded(Paths.get("2"))))
      stop.onComplete()
      whenObserved.await(99.s)
    }
  }

  "Starting observation under load" in {
    withTemporaryDirectory("DirectoryWatcherTest-") { dir =>
      val options = WatchOptions.forTest(dir, Set(ENTRY_CREATE, ENTRY_DELETE))
      val buffer = mutable.Buffer[Seq[DirectoryEvent]]()
      val whenObserved = DirectoryWatcher.observable(DirectoryState.empty, options)
        .foreach { buffer += _ }
      var first = 0

      for (n <- Seq(1000, 1, 10, 500, 3, 50)) {
        buffer.clear()
        val indices = first + 0 until first + n
        val fileCreationFuture = Observable.fromIterable(indices).executeAsync.foreach { i =>
          touch(dir / i.toString)
        }
        waitForCondition(99.s, 10.ms)(buffer.view.map(_.size).sum == indices.size)
        fileCreationFuture.await(100.ms)

        assert(buffer.flatten.sortBy(_.relativePath.getFileName.toString.toInt) ==
          indices.map(i => FileAdded(Paths.get(i.toString))))
        sleep(100.ms)
        first += n
      }

      whenObserved.cancel()
    }
  }

  "Starting observation under load with some deletions" in {
    pending
  }
}
