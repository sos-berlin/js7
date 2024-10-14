package js7.base.service

import cats.effect.testkit.TestControl
import cats.effect.unsafe.IORuntime
import cats.effect.{IO, ResourceIO}
import cats.syntax.parallel.*
import js7.base.catsutils.CatsDeadline
import js7.base.log.Logger
import js7.base.service.RestartAfterFailureServiceTest.*
import js7.base.test.OurAsyncTestSuite
import js7.base.thread.CatsBlocking.syntax.await
import js7.base.time.ScalaTime.*
import js7.base.utils.Atomic.extensions.*
import js7.base.utils.CatsUtils.syntax.RichResource
import js7.base.utils.{Atomic, DelayConf}
import js7.tester.ScalaTestUtils.awaitAndAssert
import scala.collection.mutable
import scala.concurrent.duration.*
import scala.util.Random
import scala.util.control.NoStackTrace

final class RestartAfterFailureServiceTest extends OurAsyncTestSuite:

  "RestartAfterFailureService" in:
    val elapsedSeq = mutable.Buffer[(FiniteDuration, FiniteDuration)]()

    TestControl
      .executeEmbed:
        CatsDeadline.now.flatMap: started =>
          val serviceResource: ResourceIO[RestartAfterFailureService[CancelableService]] =
            var i = 0
            var lastEnd = started

            Service.restartAfterFailure()(CancelableService.resource(
              CatsDeadline.now.flatMap: now =>
                val delayed = now - lastEnd
                i += 1
                val sleep =
                  if i < 8 then 0.s
                  else if i < 11 then 5.s
                  else if i < 12 then 11.s
                  else 0.s
                val n = 20
                logger.debug(s"$toString i=$i ${if i == n then "last" else s"sleep ${sleep.pretty}"}")
                IO.whenA(i < n):
                  IO.sleep(sleep)
                    .*>(CatsDeadline.now)
                    .flatMap: now =>
                      lastEnd = now
                      elapsedSeq += ((delayed.toCoarsest, sleep.toCoarsest))
                      IO.raiseError(new TestException("run"))))
          serviceResource
            .use: (service: RestartAfterFailureService[CancelableService]) =>
              // Due to automatic restart, the underlying service may change.
              service.unsafeCurrentService(): CancelableService
              service.untilStopped
      .map: _ =>
        assert(elapsedSeq == Seq(
          // (delayed, run duration)
          (0.s, 0.s),   // 1.  first
          (0.s, 0.s),   // 2.  0s
          (1.s, 0.s),   // 3.  1s
          (3.s, 0.s),   // 4.  3s
          (6.s, 0.s),   // 5.  6s
          (10.s, 0.s),  // 6.  10s
          (10.s, 0.s),  // 7.  10s
          (10.s, 5.s),  // 8.  10s
          (5.s, 5.s),   // 9.  10s
          (5.s, 5.s),   // 10. 10s
          (5.s, 11.s),  // 11. 10s
          (0.s, 0.s),   // 12. 0s reset because duration was >= 10s
          (0.s, 0.s),   // 13. 1s
          (1.s, 0.s),   // 14. 3s
          (3.s, 0.s),   // 15. 6s
          (6.s, 0.s),   // 16. 10s
          (10.s, 0.s),  // 17. 10s
          (10.s, 0.s),  // 18. 10s
          (10.s, 0.s))) // 19. 10s

  "RestartAfterFailureService is stoppable anytime · Memory test when test.speed" in:
    given IORuntime = ioRuntime

    // For check agains OutOfMemoryError, set -Xmx10m !!!
    val testDuration = if sys.props.contains("test.speed") then 30.s else 100.ms
    val runs = Atomic(0)
    val uniqueCounter = Atomic(0)

    final class TestService(
      startFailsRandomly: Boolean,
      runFails: Boolean,
      stopFails: Boolean,
      name: String)
    extends Service.StoppableByRequest:
      // startFailsRandomly: if start would fail every time, RestartAfterFailureService.start would
      // not terminate, and the caller has no opportunity to stop the not yet started service.
      // But start is cancelable !!!

      private lazy val unique = uniqueCounter.getAndAdd(1)

      protected def start =
        IO.defer:
          if startFailsRandomly && Random.nextBoolean() then
            IO.sleep(Random.nextInt(5).ms) *> IO.raiseError(new TestException("start"))
          else
            startService(IO
              .defer:
                runs += 1
                IO.sleep(Random.nextInt(5).ms) *> (
                  if runFails then
                    IO.raiseError(new TestException("run"))
                  else
                    untilStopRequested >>
                      IO.raiseWhen(stopFails)(new TestException("stopped")))
              .guarantee(IO:
                runs -= 1))

      override def toString = s"TestService($name $unique)"

    (0 until 8 /*2^3 == 8 combinations*/).toVector
      .parTraverse(i => Service
        .restartAfterFailure(restartDelayConf = DelayConf(0.s), runDelayConf = DelayConf(0.s))(
          Service.resource(IO(
            new TestService(
              startFailsRandomly = (i & 1) != 0,
              runFails = (i & 2) != 0,
              stopFails = (i & 4) != 0,
              i.toString))))
        .toAllocated)
      .flatTap(_ => IO.sleep(testDuration))
      .flatMap(allocatedServices => allocatedServices
        .parTraverse(_
          .release
          .handleError(t => logger.debug(s"stop => $t"))))
      .await(99.s)

    // runs > 0 probably because a race condition between stop and restart of the service.
    // TODO That means, the service may continue to run a while after stop!
    awaitAndAssert(runs.get() == 0)
    assert(runs.get() == 0)


object RestartAfterFailureServiceTest:
  private val logger = Logger[this.type]
  private class TestException(msg: String) extends RuntimeException(msg), NoStackTrace:
    override def toString = s"TestException($msg)"
