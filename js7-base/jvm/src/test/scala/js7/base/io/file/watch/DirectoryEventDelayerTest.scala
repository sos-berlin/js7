package js7.base.io.file.watch

import DirectoryEventDelayerTest.*
import cats.data.NonEmptySeq
import cats.effect.IO
import cats.effect.testkit.TestControl
import cats.syntax.flatMap.*
import java.nio.file.Paths
import js7.base.fs2utils.Fs2PubSub
import js7.base.io.file.watch.DirectoryEvent.{FileAdded, FileDeleted, FileModified}
import js7.base.io.file.watch.DirectoryEventDelayer.syntax.*
import js7.base.io.file.watch.DirectoryEventDelayerTest.*
import js7.base.log.Logger
import js7.base.test.{OurAsyncTestSuite, OurTestSuite, TestCatsEffect}
import js7.base.thread.Futures.implicits.SuccessFuture
import js7.base.time.ScalaTime.*
import js7.base.time.Stopwatch.itemsPerSecondString
import js7.base.time.WaitForCondition.waitForCondition
import js7.base.monixlike.SerialCancelable
import js7.base.test.CatsEffectTestkitExtensions.{orRaiseError, test}
import js7.base.thread.CatsBlocking.syntax.await
import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.should.Matchers.*
import scala.collection.mutable
import scala.concurrent.Future
import scala.concurrent.duration.Deadline
import scala.util.Random

final class DirectoryEventDelayerTest extends OurAsyncTestSuite, BeforeAndAfterAll:

  "DirectoryEventDelayer" in:
    Fs2PubSub.resource[IO, Seq[DirectoryEvent]]
      .use { pubSub =>
        val buffer = mutable.Buffer[DirectoryEvent]()
        val program = pubSub
          .newStream
          .flatMap(_.use(stream =>
            stream
              .delayFileAdded(Paths.get("/tmp"), delay = 2.s, logDelays = NonEmptySeq.one(0.s))
              .foreach(events => IO:
                buffer ++= events)
              .compile
              .drain))
          .as(succeed)

        TestControl.test(program).use(control =>
          pubSub.publish(Seq(aFileAdded))
            .both(control.tick
              .*>(IO(assert(buffer.isEmpty)))
              .*>(control.tickFor(2.s))
              .*>(IO(assert(buffer.toSeq == Seq(aFileAdded)))))
            .as(succeed))
      }

  //"Two FileAdded" in:
  //  ack = pubSub.onNext(bFileAdded).await(99.s)
  //  assert(ack == Continue)
  //  assert(buffer.last == aFileAdded)
  //
  //  scheduler.tick(1.s)
  //  ack = pubSub.onNext(cFileAdded).await(99.s)
  //  assert(ack == Continue)
  //  assert(buffer.last == aFileAdded)
  //
  //  scheduler.tick(1.s)
  //  assert(buffer.last == bFileAdded)
  //
  //  scheduler.tick(1.s)
  //  assert(buffer.last == cFileAdded)
  //
  //"FileModifed delays even more" in:
  //  ack = pubSub.onNext(aFileAdded).await(99.s)
  //  for _ <- 1 to 3 do
  //    scheduler.tick(1.s)
  //    ack = pubSub.onNext(aFileModified).await(99.s)
  //  scheduler.tick(1.s)
  //  assert(buffer.last == cFileAdded)
  //
  //  scheduler.tick(1.s)
  //  assert(buffer.last == aFileAdded)
  //
  //"FileDeleted is not delayed" in:
  //  buffer.clear()
  //  ack = pubSub.onNext(aFileAdded).await(99.s)
  //  ack = pubSub.onNext(bFileAdded).await(99.s)
  //  ack = pubSub.onNext(bFileDeleted).await(99.s)
  //  assert(buffer == Seq(/*bFileAdded, bFileDeleted*/))
  //  scheduler.tick(1.s)
  //  assert(buffer == Seq(/*bFileAdded, bFileDeleted*/))
  //  scheduler.tick(1.s)
  //  assert(buffer == Seq(/*bFileAdded, bFileDeleted,*/ aFileAdded))
  //
  //  // Single FileDeleted without a currently delayed FileAdded
  //  buffer.clear()
  //  ack = pubSub.onNext(bFileDeleted).await(99.s)
  //  assert(buffer == Seq(bFileDeleted))
  //
  //"Crash test" in:
  //  implicit val scheduler = Scheduler.traced
  //  val buffer = mutable.Buffer[DirectoryEvent]()
  //  val publishSubject = PublishSubject[DirectoryEvent]()
  //  val future = publishSubject
  //    .delayFileAdded(Paths.get("/tmp"), 100.ms, logDelays = NonEmptySeq.one(0.s))
  //    .bufferIntrospective(2)  // Let onNext complete synchronously sometimes
  //    .map(_.flatten)
  //    .delayOnNext(1.ms)
  //    .foreach(buffer ++= _)
  //  var ack: Ack = Continue
  //  val paths = (1 to 10000).map(i => Paths.get(i.toString))
  //
  //  val addedEvents = paths.map(FileAdded(_))
  //  val addedSince = Deadline.now
  //  for event <- addedEvents do
  //    ack.syncOnContinue(publishSubject.onNext(event).await(9.s))
  //      .await(9.s) shouldBe Continue
  //    sleep(Random.nextInt(2).ms)
  //  assert(ack == Continue)
  //  waitForCondition(99.s, 10.ms)(buffer.size == addedEvents.size)
  //  logger.info(itemsPerSecondString(addedSince.elapsed, paths.size, "FileAdded"))
  //
  //  val deletedEvents = paths.map(FileDeleted(_))
  //  val deletedSince = Deadline.now
  //  for event <- deletedEvents do
  //    ack.syncOnContinue(publishSubject.onNext(event).await(9.s))
  //      .await(9.s) shouldBe Continue
  //  assert(ack == Continue)
  //
  //  ack = ack.syncOnContinue:
  //    publishSubject.onComplete()
  //  ack.await(99.s)
  //  future.await(99.s)
  //  logger.info(itemsPerSecondString(deletedSince.elapsed, paths.size, "FileDeleted"))
  //  assert(buffer == addedEvents ++ deletedEvents)


object DirectoryEventDelayerTest:
  private val logger = Logger[this.type]

  private val aFileAdded = FileAdded(Paths.get("A"))
  private val aFileModified = FileModified(Paths.get("A"))
  private val bFileAdded = FileAdded(Paths.get("B"))
  private val bFileDeleted = FileDeleted(Paths.get("B"))
  private val cFileAdded = FileAdded(Paths.get("C"))
