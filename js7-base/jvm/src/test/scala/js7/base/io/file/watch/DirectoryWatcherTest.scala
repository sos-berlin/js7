package js7.base.io.file.watch

import java.nio.file.Files.{createDirectory, delete}
import java.nio.file.Paths
import java.nio.file.StandardWatchEventKinds.{ENTRY_CREATE, ENTRY_DELETE}
import js7.base.io.file.FileUtils.syntax.*
import js7.base.io.file.FileUtils.{touchFile, withTemporaryDirectory}
import js7.base.io.file.watch.DirectoryEvent.{FileAdded, FileDeleted}
import js7.base.io.file.watch.DirectoryState.Entry
import js7.base.log.ScribeForJava.coupleScribeWithSlf4j
import js7.base.test.OurTestSuite
import js7.base.thread.Futures.implicits.SuccessFuture
import js7.base.thread.IOExecutor.Implicits.globalIOX
import js7.base.thread.MonixBlocking.syntax.RichTask
import js7.base.time.ScalaTime.*
import js7.tester.ScalaTestUtils.awaitAndAssert
import monix.eval.Task
import monix.execution.Scheduler.Implicits.traced
import monix.reactive.Observable
import monix.reactive.subjects.PublishSubject
import scala.concurrent.Promise
import scala.math.Ordering.Int

final class DirectoryWatcherTest extends OurTestSuite
{
  coupleScribeWithSlf4j()

  "readDirectory, readDirectoryAsEvents" in {
    withTemporaryDirectory("DirectoryWatcherTest-") { dir =>
      touchFile(dir / "TEST-1")
      touchFile(dir / "IGNORED")
      touchFile(dir / "TEST-2")
      val state = DirectoryState.readDirectory(dir, _.toString startsWith "TEST-")
      assert(state == DirectoryState(Map(
        Paths.get("TEST-1") -> DirectoryState.Entry(Paths.get("TEST-1")),
        Paths.get("TEST-2") -> DirectoryState.Entry(Paths.get("TEST-2")))))

      touchFile(dir / "TEST-A")
      touchFile(dir / "TEST-1")

      assert(state.diffTo(DirectoryState.readDirectory(dir, _.toString startsWith "TEST-")).toSet ==
        Set(FileAdded(Paths.get("TEST-A"))))
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
      toDirectoryState(files*)

    val watcher = new DirectoryWatcher(Task(readDirectory()), directoryEvents, 1.s)

    def observe(state: DirectoryState, n: Int) =
      watcher.readDirectoryAndObserve(state)
        .take(n).toListL.await(99.s)

    var state = readDirectory()
    addFile("TEST-1")
    assert(observe(state, 1) == Seq(
      Seq(FileAdded(Paths.get("TEST-1"))) -> toDirectoryState("0", "TEST-1")))

    // The duplicate event will be ignored
    directoryEvents.onNext(Seq(FileAdded(Paths.get("TEST-1")))) await 99.s

    state = readDirectory()
    addFile("TEST-2")
    assert(observe(state, 1) == Seq(
      Seq(FileAdded(Paths.get("TEST-2"))) -> toDirectoryState("0", "TEST-1", "TEST-2")))
  }

  "observe" in {
    withTemporaryDirectory("DirectoryWatcherTest-") { dir =>
      val options = WatchOptions.forTest(dir, Set(ENTRY_CREATE, ENTRY_DELETE))
      var buffer = Vector.empty[Set[DirectoryEvent]]
      val subscribed = Promise[Unit]()
      val stop = PublishSubject[Unit]()
      val whenObserved = DirectoryWatcher.observable(DirectoryState.empty, options)
        .doOnSubscribe(Task(subscribed.success(())))
        .takeUntil(stop)
        .foreach(buffer :+= _.toSet)
      subscribed.future await 99.s
      assert(buffer.isEmpty)
      touchFile(dir / "TEST-1")
      awaitAndAssert(buffer == Seq(Set(FileAdded(Paths.get("TEST-1")))))
      stop.onComplete()
      whenObserved.await(99.s)
    }
  }

  "Observing a deleted and recreated directory" in {
    withTemporaryDirectory("DirectoryWatcherTest-") { mainDir =>
      val dir = mainDir / "DIRECTORY"
      val options = WatchOptions.forTest(dir, Set(ENTRY_CREATE, ENTRY_DELETE), pollTimeout = 100.ms)
      var buffer = Vector.empty[DirectoryEvent]
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
      touchFile(dir / "TEST-1")
      awaitAndAssert(buffer contains FileAdded(Paths.get("TEST-1")))
      delete(dir / "TEST-1")

      delete(dir)
      sleep(300.ms)
      createDirectory(dir)
      touchFile(dir / "TEST-2")
      awaitAndAssert(buffer.contains(FileDeleted(Paths.get("TEST-1")))
        && buffer.contains(FileAdded(Paths.get("TEST-2"))))
      stop.onComplete()
      whenObserved.await(99.s)
    }
  }

  "Starting observation under load" in {
    withTemporaryDirectory("DirectoryWatcherTest-") { dir =>
      val options = WatchOptions.forTest(dir, Set(ENTRY_CREATE, ENTRY_DELETE))
      var buffer = Vector.empty[Seq[DirectoryEvent]]
      val whenObserved = DirectoryWatcher.observable(DirectoryState.empty, options)
        .foreach(buffer :+= _)
      var first = 0

      for (n <- Seq(1000, 1, 10, 500, 3, 50)) {
        buffer = Vector.empty
        val indices = first + 0 until first + n
        val fileCreationFuture = Observable.fromIterable(indices).executeAsync.foreach { i =>
          touchFile(dir / i.toString)
        }
        awaitAndAssert(buffer.view.map(_.size).sum == indices.size)
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
