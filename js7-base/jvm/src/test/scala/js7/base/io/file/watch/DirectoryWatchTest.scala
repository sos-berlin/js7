package js7.base.io.file.watch

import cats.effect.unsafe.IORuntime
import cats.effect.{Deferred, IO}
import fs2.Stream
import java.nio.file.Files.{createDirectory, delete}
import java.nio.file.Paths
import js7.base.fs2utils.Fs2PubSub
import js7.base.fs2utils.StreamExtensions.*
import js7.base.io.file.FileUtils
import js7.base.io.file.FileUtils.syntax.*
import js7.base.io.file.FileUtils.{temporaryDirectoryResource, touchFile}
import js7.base.io.file.watch.DirectoryEvent.{FileAdded, FileDeleted}
import js7.base.io.file.watch.DirectoryState.Entry
import js7.base.io.file.watch.DirectoryWatchTest.*
import js7.base.log.Logger
import js7.base.test.OurAsyncTestSuite
import js7.base.thread.Futures.implicits.SuccessFuture
import js7.base.time.ScalaTime.*
import js7.tester.ScalaTestUtils.awaitAndAssert
import scala.math.Ordering.Int

final class DirectoryWatchTest extends OurAsyncTestSuite:

  private given IORuntime = ioRuntime

  "readDirectory, readDirectoryAsEvents" in:
    temporaryDirectoryResource[IO]("DirectoryWatchTest-").use: dir =>
      touchFile(dir / "TEST-1")
      touchFile(dir / "IGNORED")
      touchFile(dir / "TEST-2")
      for
        state <- DirectoryStateJvm.readDirectory(dir, _.toString.startsWith("TEST-"))
        _ = assert(state ==
          DirectoryState(Map(
            Paths.get("TEST-1") -> DirectoryState.Entry(Paths.get("TEST-1")),
            Paths.get("TEST-2") -> DirectoryState.Entry(Paths.get("TEST-2")))))
        _ = touchFile(dir / "TEST-A")
        _ = touchFile(dir / "TEST-1")
        state2 <- DirectoryStateJvm.readDirectory(dir, _.toString.startsWith("TEST-"))
      yield
        assert(state.diffTo(state2).toSet == Set(FileAdded(Paths.get("TEST-A"))))

  "readDirectoryThenStream" in:
    Fs2PubSub
      .resource[IO, Seq[DirectoryEvent]]
      .use(publisher => IO.defer:
        @volatile var files = Seq("0")

        def addFile(name: String): IO[Unit] =
          files :+= name
          publisher.publish(Seq(FileAdded(Paths.get(name))))

        def toDirectoryState(names: String*) =
          DirectoryState.fromIterable(names.map(Paths.get(_)).map(Entry(_)))

        def readDirectory() =
          toDirectoryState(files*)

        def observe(state: DirectoryState, n: Int)
        : IO[List[(Seq[DirectoryEvent], DirectoryState)]] =
          publisher.streamResource.use(stream =>
            new DirectoryWatch(IO(readDirectory()), stream, 1.s)
              .readDirectoryThenStream(state)
              .take(n).compile.toList)

        var state = readDirectory()
        for
          _ <- addFile("TEST-1")
          events <- observe(state, 1)
          _ = assert:
            events == Seq(
              Seq(FileAdded(Paths.get("TEST-1"))) -> toDirectoryState("0", "TEST-1"))
          _ <- // The duplicate event will be ignored
            publisher.publish(Seq(FileAdded(Paths.get("TEST-1"))))
          _ <-
            state = readDirectory()
            addFile("TEST-2")
          events <- observe(state, 1)
        yield assert:
          events == Seq(
            Seq(FileAdded(Paths.get("TEST-2"))) -> toDirectoryState("0", "TEST-1", "TEST-2")))

  "stream" in:
    temporaryDirectoryResource[IO]("DirectoryWatchTest-")
      .use(dir => IO.defer:
        var buffer = Vector.empty[Set[DirectoryEvent]]
        val stop = Deferred.unsafe[IO, Unit]
        val started = Deferred.unsafe[IO, Unit]
        DirectoryWatch
          .stream(dir, DirectoryState.empty, DirectoryWatchSettings.forTest())
          .onStart(started.complete(()).void)
          .interruptWhenF(stop.get)
          .chunks
          .foreach(chunk => IO:
            buffer :+= chunk.to(Set))
          .compile
          .drain
          .both(started.get
            .*>(IO.interruptible:
              assert(buffer.isEmpty)
              touchFile(dir / "TEST-1")
              awaitAndAssert:
                buffer == Seq(Set(FileAdded(Paths.get("TEST-1")))))
            .*>(stop.complete(())))
          .as(succeed))

  "stream, while directory is being deleted and recreated" in:
    temporaryDirectoryResource[IO]("DirectoryWatchTest-")
      .use(mainDir => IO.defer:
        val dir = mainDir / "DIRECTORY"
        var buffer = Vector.empty[DirectoryEvent]
        val started = Deferred.unsafe[IO, Unit]
        val stop = Deferred.unsafe[IO, Unit]
        DirectoryWatch
          .stream(dir, DirectoryState.empty, DirectoryWatchSettings.forTest(pollTimeout = 100.ms))
          .onStart(started.complete(()).void)
          .interruptWhenF(stop.get)
          .chunks
          .foreach(chunk => IO:
            buffer ++= chunk.to(Seq))
          .compile
          .drain
          .*>(IO(logger.info("drained")))
          .both(
            IO(logger.info(s"Second fiber"))
              .*>(IO.sleep(500.ms)) // Delay directory creation
              .*>(IO.defer:
                createDirectory(dir)
                started.get)
              .*>(IO(logger.info(s"started")))
              .*>(IO.interruptible:
                assert(buffer.isEmpty)
                touchFile(dir / "TEST-1")
                awaitAndAssert:
                  buffer contains FileAdded(Paths.get("TEST-1"))
                logger.info(s"delete TEST-1")
                delete(dir / "TEST-1")

                delete(dir)
                sleep(300.ms)
                createDirectory(dir)
                touchFile(dir / "TEST-2")
                logger.info(s"delete TEST-2 touched")
                awaitAndAssert:
                  buffer.contains(FileDeleted(Paths.get("TEST-1")))
                    && buffer.contains(FileAdded(Paths.get("TEST-2")))
                logger.info("stop"))
              .*>(stop.complete(())))
      .as(succeed))

  "Starting observation under load" in:
    temporaryDirectoryResource[IO]("DirectoryWatchTest-")
      .use(dir => IO.defer:
        var buffer = Vector.empty[Seq[DirectoryEvent]]
        DirectoryWatch
          .stream(dir, DirectoryState.empty, DirectoryWatchSettings.forTest())
          .chunks
          .foreach(chunk => IO(buffer :+= chunk.to(Seq)))
          .compile
          .drain
          .racePair(IO.interruptible:
            var first = 0

            for n <- Seq(1000, 1, 10, 500, 3, 50) do
              buffer = Vector.empty
              val indices = first + 0 until first + n
              val fileCreationFuture = Stream.emits(indices).covary[IO]
                .foreach(i => IO:
                  touchFile(dir / i.toString))
                .compile
                .drain
                .unsafeToFuture()
              awaitAndAssert(99.s):
                buffer.view.map(_.size).sum == indices.size
              fileCreationFuture.await(100.ms)
              assert(buffer.flatten.sortBy(_.relativePath.getFileName.toString.toInt) ==
                indices.map(i => FileAdded(Paths.get(i.toString))))
              sleep(100.ms)
              first += n)
          .flatMap:
            case Left((outcome, fiber)) => IO(fail())
            case Right((fiber, outcome)) => fiber.cancel.*>(outcome.embedError).as(succeed))

  "Starting observation under load with some deletions" in:
    pending

object DirectoryWatchTest:
  private val logger = Logger[this.type]
