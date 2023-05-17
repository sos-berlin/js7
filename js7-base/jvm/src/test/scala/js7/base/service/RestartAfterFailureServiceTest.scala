package js7.base.service

import cats.effect.{Resource, Timer}
import cats.implicits.catsSyntaxApplicativeError
import cats.syntax.flatMap.*
import cats.syntax.parallel.*
import js7.base.log.Logger
import js7.base.monixutils.MonixDeadline.now
import js7.base.service.RestartAfterFailureServiceTest.*
import js7.base.test.OurTestSuite
import js7.base.thread.Futures.implicits.SuccessFuture
import js7.base.thread.MonixBlocking.syntax.RichTask
import js7.base.time.ScalaTime.*
import js7.base.time.WaitForCondition.waitForCondition
import js7.base.utils.CatsUtils.syntax.RichResource
import monix.eval.Task
import monix.execution.Scheduler
import monix.execution.atomic.Atomic
import monix.execution.schedulers.TestScheduler
import scala.collection.mutable
import scala.concurrent.duration.*
import scala.util.Random
import scala.util.control.NoStackTrace

final class RestartAfterFailureServiceTest extends OurTestSuite
{
  "RestartAfterFailureService" in {
    implicit val scheduler = TestScheduler()
    val started = now
    val elapsedSeq = mutable.Buffer[(FiniteDuration, FiniteDuration)]()

    def serviceResource: Resource[Task, RestartAfterFailureService[CancelableService]] = {
      var i = 0
      var lastEnd = started

      Service.restartAfterFailure()(CancelableService.resource(
        Task.defer {
          val delayed = now - lastEnd
          i += 1
          val sleep =
            if (i < 8) 0.s
            else if (i < 11) 5.s
            else if (i < 12) 11.s
            else 0.s
          val n = 20
          logger.debug(s"$toString $now i=$i ${if (i == n) "last" else s"sleep ${sleep.pretty}"}")
          Task.when(i < n)(
            Task.sleep(sleep) *>
              Task.defer {
                lastEnd = now
                elapsedSeq += ((delayed.toCoarsest, sleep.toCoarsest))
                Task.raiseError(new TestException("run"))
              })
        }))
    }

    val running = serviceResource
      .use { (service: RestartAfterFailureService[CancelableService]) =>
        // Due to automatic restart, the underlying service may change.
        service.unsafeCurrentService(): CancelableService
        service.untilStopped
      }
      .runToFuture

    scheduler.tick(900.s)
    running.await(99.s)

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
  }

  "RestartAfterFailureService is stoppable anytime · Memory test when test.speed" in {
    // For check agains OutOfMemoryError, set -Xmx10m !!!
    val testDuration = if (sys.props.contains("test.speed")) 30.s else 100.ms
    import Scheduler.Implicits.traced
    val timer = implicitly[Timer[Task]]
    val runs = Atomic(0)
    val uniqueCounter = Atomic(0)

    final class TestService(
      startFailsRandomly: Boolean,
      runFails: Boolean,
      stopFails: Boolean,
      name: String)
    extends Service.StoppableByRequest {
      // startFailsRandomly: if start would fail every time, RestartAfterFailureService.start would
      // not terminate, and the caller has no opportunity to stop the not yet started service.
      // But start is cancelable !!!

      private val unique = uniqueCounter.getAndIncrement(1)

      protected def start =
        Task.defer {
          if (startFailsRandomly && Random.nextBoolean())
            timer.sleep(Random.nextInt(5).ms) *> Task.raiseError(new TestException("start"))
          else
            startService(Task
              .defer {
                runs += 1
                timer.sleep(Random.nextInt(5).ms) *> (
                  if (runFails)
                    Task.raiseError(new TestException("run"))
                  else
                    untilStopRequested >>
                      Task.raiseWhen(stopFails)(new TestException("stopped")))
              }
              .guarantee(Task {
                runs -= 1
              }))
        }

      override def toString = s"TestService($name $unique)"
    }

    (0 until 8 /*2^3 == 8 combinations*/).toVector
      .parTraverse(i => Service
        .restartAfterFailure(startDelays = Seq(0.s), runDelays = Seq(0.s))(
          Service.resource(Task(
            new TestService(
              startFailsRandomly = (i & 1) != 0,
              runFails = (i & 2) != 0,
              stopFails = (i & 4) != 0,
              i.toString))))
        .toAllocated)
      .flatTap(_ => Task.sleep(testDuration))
      .flatMap(allocatedServices => allocatedServices
        .parTraverse(_
          .release
          .handleError(t => logger.debug(s"stop => $t"))))
      .await(99.s)

    // runs > 0 probably because a race condition between stop and restart of the service.
    // TODO That means, the service may continue to run a while after stop!
    waitForCondition(10.s, 10.ms)(runs.get() == 0)
    assert(runs.get() == 0)
  }
}

object RestartAfterFailureServiceTest {
  private val logger = Logger[this.type]
  private class TestException(msg: String) extends RuntimeException(msg) with NoStackTrace {
    override def toString = s"TestException($msg)"
  }
}
