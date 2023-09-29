package js7.base.service

import cats.effect.Resource
import cats.effect.concurrent.Deferred
import js7.base.log.Logger
import js7.base.log.Logger.syntax.*
import js7.base.service.ServiceTest.*
import js7.base.test.OurAsyncTestSuite
import js7.base.time.ScalaTime.DurationRichInt
import js7.base.utils.Tests.isIntelliJIdea
import monix.eval.Task
import monix.execution.Scheduler.Implicits.traced
import org.scalatest.Assertion
import scala.concurrent.Future
import scala.util.Failure
import scala.util.control.NoStackTrace

final class ServiceTest extends OurAsyncTestSuite
{
  private val delay = 200.ms
  private val iterations = if isIntelliJIdea then 100 else 10

  private def repeat(n: Int)(body: => Future[Assertion]): Future[Assertion] =
    Task.tailRecM(1)(i => logger
      .debugTask(s"#$i")(Task
        .deferFuture(
          withClue(s"#$i: ")(
            body)))
      .map(result =>
        if i < n then
          Left(i + 1)
        else
          Right(result)))
      .runToFuture

  "Resource.release does not suppresses exceptions" in {
    Resource
      .make(Task.unit)(_ => Task.raiseError(new IllegalStateException))
      .use(_ => Task(succeed))
      .materialize
      .flatMap {
        case Failure(_: IllegalStateException) => Task(succeed)
        case other => fail(s"UNEXPECTED: $other")
      }
      .runToFuture
  }

  "Service terminates while still in use" in repeat(iterations) {
    var isRunning = false
    MyService.resource(isRunning = _)
      .use(service =>
        // service fails while in use!
        service.running.get *>
          service.stop *>
          Task(assert(!isRunning))
            .as(succeed))
      .runToFuture
  }

  "Service fails while still in use" in repeat(iterations) {
    val serviceFailed = Deferred.unsafe[Task, Unit]
    FailingService
      .resource(onFailed = serviceFailed.complete(()))
      .use(_ => serviceFailed.get.delayExecution(delay))
      .materialize
      .map {
        case Failure(FailingService.exception) =>
          succeed
        case o => fail(s"Unexpected $o")
      }
      .runToFuture
  }

  "Use ends when service has stopped" in repeat(iterations) {
    // General use case. Similar to the test above.
    val serviceFailed = Deferred.unsafe[Task, Unit]
    FailingService
      .resource(onFailed = serviceFailed.complete(()))
      .use(_.untilStopped)
      .materialize
      .map {
        case Failure(FailingService.exception) =>
          succeed
        case o => fail(s"Unexpected $o")
      }
      .runToFuture
  }

  "Service usage terminates before service would die" in repeat(iterations) {
    val serviceFailed = Deferred.unsafe[Task, Unit]
    FailingService
      .resource(
        whenFail = Task.sleep(delay),
        onFailed = serviceFailed.complete(()))
      .use(_ => Task.pure(succeed))
      .runToFuture
  }

  "Service fails while usage is terminating (race)" in repeat(iterations) {
    val failService = Deferred.unsafe[Task, Unit]
    FailingService
      .resource(whenFail = failService.get)
      .use(_ =>
        // service fails while in use!
        failService.complete(()).as(succeed))
      .onErrorRecover {
        case FailingService.exception => succeed
      }
      .runToFuture
  }

  "No failure " in repeat(iterations) {
    var isRunning = false
    MyService.resource(isRunning = _)
      .use(service =>
        service.running.get
          .flatMap(_ => Task {
            assert(isRunning)
          }))
      .tapEval(_ => Task {
        assert(!isRunning)
      })
      .runToFuture
  }

  private class MyService(setRunning: Boolean => Unit)
  extends Service.StoppableByRequest
  {
    val running = Deferred.unsafe[Task, Unit]

    override def stop = super.stop

    protected def start =
      startService(Task.defer {
        setRunning(true)
        running
          .complete(())
          .*>(untilStopRequested)
          .guaranteeCase(exitCase => Task {
            logger.info(s"$exitCase")
            setRunning(false)
          })
      })

    override def toString = "MyService"
  }
  private object MyService {
    def resource(setRunning: Boolean => Unit): Resource[Task, MyService] =
      Service.resource(Task(new MyService(setRunning)))
  }
}

object ServiceTest
{
  private val logger = Logger[this.type]

  private class FailingService(
    whenFail: Task[Unit],
    onFailed: Task[Unit])
  extends Service.StoppableByRequest
  {
    protected def start =
      startService(
        Task.race(run2, untilStopRequested).void)

    private def run2 =
      whenFail *> Task.raiseError(FailingService.exception).guarantee(onFailed)

    override def toString = "FailingService"
  }
  private object FailingService {
    val exception = new Exception("SERVICE FAILED") with NoStackTrace

    def resource(
      whenFail: Task[Unit] = Task.unit,
      onFailed: Task[Unit] = Task.unit)
    = Service.resource(Task(new FailingService(whenFail, onFailed)))
  }
}
