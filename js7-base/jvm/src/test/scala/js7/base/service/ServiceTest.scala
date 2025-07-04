package js7.base.service

import cats.effect.{Deferred, IO, Resource, ResourceIO}
import js7.base.log.Logger
import js7.base.service.ServiceTest.*
import js7.base.test.OurAsyncTestSuite
import js7.base.time.ScalaTime.DurationRichInt
import js7.base.utils.Atomic
import js7.base.utils.Atomic.extensions.*
import js7.base.utils.ScalaUtils.syntax.RichThrowable
import js7.base.utils.Tests.isIntelliJIdea
import scala.util.control.NoStackTrace

final class ServiceTest extends OurAsyncTestSuite:

  private val delay = 200.ms
  private val iterations = if isIntelliJIdea then 100 else 10

  "Resource.release does not suppress exceptions" in:
    Resource
      .make(IO.unit)(_ => IO.raiseError(new IllegalStateException))
      .surround(IO(succeed))
      .attempt
      .flatMap:
        case Left(_: IllegalStateException) => IO(succeed)
        case other => fail(s"UNEXPECTED: $other")

  "Service terminates while still in use" in repeatTest(iterations): _ =>
    var isRunning = false
    MyService.resource(isRunning = _)
      .use: service =>
        // service fails while in use!
        service.running.get *>
          service.stop *>
          IO(assert(!isRunning))
            .as(succeed)

  "Service fails while still in use" in repeatTest(iterations): _ =>
    val serviceFailed = Deferred.unsafe[IO, Unit]
    FailingService
      .resource(onFailed = serviceFailed.complete(()).void)
      .use: service =>
        serviceFailed.get.delayBy(delay) *> service.untilStopped
      .attempt
      .map:
        case Left(FailingService.exception) => succeed
        case o => fail(s"Unexpected $o")

  "Use ends when service has stopped" in repeatTest(iterations): _ =>
    // General use case. Similar to the test above.
    val serviceFailed = Deferred.unsafe[IO, Unit]
    FailingService
      .resource(onFailed = serviceFailed.complete(()).void)
      .use(_.untilStopped)
      .attempt
      .map:
        case Left(FailingService.exception) => succeed
        case o => fail(s"Unexpected $o")

  "Service usage terminates before service would die" in repeatTest(iterations): _ =>
    val serviceFailed = Deferred.unsafe[IO, Unit]
    FailingService
      .resource(
        whenFail = IO.sleep(delay),
        onFailed = serviceFailed.complete(()).void)
      .use(_ => IO.pure(succeed))

  "Service fails while usage is terminating (race)" in repeatTest(iterations): _ =>
    val failService = Deferred.unsafe[IO, Unit]
    FailingService
      .resource(whenFail = failService.get)
      .surround:
        // service fails while in use!
        failService.complete(()).as(succeed)
      .recover:
        case FailingService.exception => succeed

  "No failure " in repeatTest(iterations): _ =>
    var isRunning = false
    MyService.resource(isRunning = _)
      .use: service =>
        service.running.get
          .<*(IO:
            assert(isRunning))
      .*>(IO:
        assert(!isRunning))

  "Service.StoppableByCancel" in:
    val started = Deferred.unsafe[IO, Unit]
    val canceled = Atomic(false)
    class CancelableService extends Service.StoppableByCancel:
      protected def start = startService:
        started.complete(()) *>
          IO.never.onCancel(IO:
            canceled := true)

    Service.resource(new CancelableService)
      .surround:
        started.get.andWait(10.ms) // Wait for IO.never
      .map: _ =>
        assert(canceled.get)


  "failWhenStopped" - {
    "terminating Service cancels the nested body" in repeatTest(iterations): _ =>
      MyService.resource(_ => ())
        .use: service =>
          service.failWhenStopped:
            service.stop *> IO.never
        .attempt.map:
          case Left(exception)
            if exception.toStringWithCauses == "MyService terminated unexpectedly" => succeed
          case x => fail(x.toString)

    "failing Service cancels the nested body" in repeatTest(iterations): _ =>
      val failService = Deferred.unsafe[IO, Unit]
      FailingService.resource(whenFail = failService.get)
        .use: service =>
          service.failWhenStopped:
            failService.complete(()) *> IO.never
        .attempt.map:
          case Left(FailingService.exception) => succeed
          case x => fail(x.toString)
  }

  private class MyService(setRunning: Boolean => Unit)
  extends Service.StoppableByRequest:
    val running = Deferred.unsafe[IO, Unit]

    override def stop = super.stop

    protected def start =
      startService:
        IO.defer:
          setRunning(true)
          running
            .complete(())
            .productR:
              untilStopRequested
            .guaranteeCase: exitCase =>
              IO:
               logger.info(s"$exitCase")
                setRunning(false)


    override def toString = "MyService"

  private object MyService:
    def resource(setRunning: Boolean => Unit): ResourceIO[MyService] =
      Service.resource(new MyService(setRunning))


object ServiceTest:
  private val logger = Logger[this.type]

  private class FailingService(whenFail: IO[Unit], onFailed: IO[Unit])
  extends Service.StoppableByRequest:
    protected def start =
      startService:
        IO.race(run2, untilStopRequested).void

    private def run2 =
      whenFail *> IO.raiseError(FailingService.exception).guarantee(onFailed)

    override def toString = "FailingService"

  private object FailingService:
    val exception = new Exception("SERVICE FAILED") with NoStackTrace

    def resource(
      whenFail: IO[Unit] = IO.unit,
      onFailed: IO[Unit] = IO.unit)
    = Service.resource(FailingService(whenFail, onFailed))
