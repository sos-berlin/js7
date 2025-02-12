package js7.base.io.file.watch

import cats.effect.IO
import cats.effect.std.Queue
import cats.effect.testkit.TestControl
import cats.instances.vector.*
import cats.syntax.traverse.*
import fs2.Stream
import java.nio.file.Paths
import js7.base.io.file.watch
import js7.base.io.file.watch.DirectoryEvent.{FileAdded, FileDeleted, FileModified}
import js7.base.io.file.watch.DirectoryEventDelayerTest.*
import js7.base.log.Logger
import js7.base.test.OurAsyncTestSuite
import js7.base.test.TestControlExtensions.*
import js7.base.time.ScalaTime.*
import js7.base.time.Stopwatch.itemsPerSecondString
import js7.base.utils.DelayConf
import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.should.Matchers.*
import scala.collection.mutable
import scala.concurrent.duration.Deadline

final class DirectoryEventDelayerTest extends OurAsyncTestSuite, BeforeAndAfterAll:

  "One added file" in:
    testDirectoryEventDelayer((feed, interimResult, control) =>
      for
        _ <- feed(aFileAdded)
        _ <- for r <- control.results yield assert(r.isEmpty && interimResult.isEmpty)
        _ <- control.tickFor(2.s)
        _ <- for r <- control.results yield assert:
          r.isEmpty && interimResult.toSeq == Seq(aFileAdded)
      yield succeed)

  "Two added files" in:
    testDirectoryEventDelayer((feed, interimResult, control) =>
      for
        _ <- feed(aFileAdded)
        _ <- control.tickFor(1.s)
        _ = assert(interimResult.isEmpty)

        _ <- feed(bFileAdded)
        _ <- control.tickFor(1.s)
        _ = assert(interimResult == Seq(aFileAdded))

        _ <- control.tickFor(1.s)
        _ = assert(interimResult == Seq(aFileAdded, bFileAdded))
      yield succeed)

  "FileModifed delays even more" in:
    testDirectoryEventDelayer((feed, interimResult, control) =>
      for
        _ <- feed(aFileAdded)
        _ <- feed(bFileAdded)
        _ <- feed(cFileAdded)
        _ <- feed(aFileModified)
        _ <- control.tickFor(1.s)
        _ = assert(interimResult.isEmpty)

        _ <- feed(aFileModified)
        _ <- feed(bFileModified)
        _ = assert(interimResult.isEmpty)

        _ <- control.tickFor(1.s) // cFileAdded is mature
        _ = assert(interimResult == Seq(cFileAdded))

        _ <- feed(bFileModified)
        _ <- control.tickFor(1.s)
        _ = assert(interimResult == Seq(cFileAdded, aFileAdded))

        _ <- feed(bFileModified)
        _ <- control.tickFor(1.s)
        _ = assert(interimResult == Seq(cFileAdded, aFileAdded))

        _ <- feed(bFileModified)
        _ <- control.tickFor(1.s)
        _ = assert(interimResult == Seq(cFileAdded, aFileAdded))

        _ <- control.tickFor(1.s) // bFileAdded is mature
        _ = assert(interimResult == Seq(cFileAdded, aFileAdded, bFileAdded))

        _ <- feed(bFileModified) // After delay has elapsed
        _ = assert(interimResult == Seq(cFileAdded, aFileAdded, bFileAdded, bFileModified))
      yield succeed)

  "FileDeleted and FileModified without FileAdded are not delayed" in:
    testDirectoryEventDelayer((feed, interimResult, control) =>
      for
        _ <- feed(aFileAdded)
        _ <- feed(bFileAdded)
        _ <- feed(cFileAdded)
        _ = assert(interimResult.isEmpty)

        _ <- control.tickFor(1.s)
        _ = assert(interimResult.isEmpty)

        _ <- feed(bFileDeleted) // Suppresses aFileAdded because delay has not elapsed
        _ = assert(interimResult.isEmpty)

        _ <- control.tickFor(1.s)
        _ = assert(interimResult == Seq(aFileAdded, cFileAdded))

        _ <- feed(aFileDeleted)
        _ = assert(interimResult == Seq(aFileAdded, cFileAdded, aFileDeleted))

        _ <- feed(aFileAdded)
        _ <- feed(bFileAdded)
        _ <- control.tickFor(1.s)
        _ = assert(interimResult == Seq(aFileAdded, cFileAdded, aFileDeleted))

        _ <- control.tickFor(1.s)
        _ = assert(interimResult == Seq(aFileAdded, cFileAdded, aFileDeleted, aFileAdded, bFileAdded))
      yield succeed)

  private def testDirectoryEventDelayer[A](
    body: (DirectoryEvent => IO[Unit], => Seq[DirectoryEvent], TestControl[Unit]) => IO[A])
  : IO[A] =
    IO.defer:
      val buffer = mutable.Buffer[DirectoryEvent]()
      Queue.unbounded[IO, DirectoryEvent | EOF].flatMap: queue =>

        val program: IO[Unit] =
          IO.defer:
            Stream.fromQueueUnterminated(queue, limit = 1)
              .takeWhile(_ != EOF)
              .collect:
                case e: watch.DirectoryEvent => e
              .through(
                DirectoryEventDelayer(Paths.get("/tmp"), delay = 2.s,
                  delayConf = DelayConf(0.s)))
              .foreach(event => IO:
                buffer += event)
              .compile
              .drain

        for
          control <- TestControl.execute(program)
            _ <- control.tick
            r <- body(o => queue.offer(o) *> control.tick, buffer.toSeq, control)
            _ <- queue.offer(EOF)
            _ <- control.finish
        yield r

  "Many FileAdded events" in:
    val addedEvents = (1 to 100000).toVector.map(i => Paths.get(i.toString)).map(FileAdded(_))
    val addedSince = Deadline.now
    val buffer = mutable.Buffer[DirectoryEvent]()
    for
      queue <- Queue.unbounded[IO, Option[DirectoryEvent]]
      streaming <- Stream.fromQueueNoneTerminated(queue, limit = 2)
        .through(
          DirectoryEventDelayer(Paths.get("/tmp/?"), delay = 100.ms,
            delayConf = DelayConf(0.s)))
        .foreach(event => IO:
          buffer += event)
        .compile.drain.start
      _ <- addedEvents
        .traverse: added =>
          queue.offer(Some(added))/*.andWait(Random.nextInt(1).ms)*/
        .guarantee(queue.offer(None))
      _ <- streaming.joinWithUnit
    yield
      logger.info(itemsPerSecondString(addedSince.elapsed, addedEvents.size, "FileAdded"))
      assert(buffer == addedEvents)


object DirectoryEventDelayerTest:
  private val logger = Logger[this.type]

  private val aFileAdded = FileAdded(Paths.get("A"))
  private val aFileDeleted = FileDeleted(Paths.get("A"))
  private val aFileModified = FileModified(Paths.get("A"))

  private val bFileAdded = FileAdded(Paths.get("B"))
  private val bFileModified = FileModified(Paths.get("B"))
  private val bFileDeleted = FileDeleted(Paths.get("B"))

  private val cFileAdded = FileAdded(Paths.get("C"))

  private type EOF = EOF.type
  private object EOF
