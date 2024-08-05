package js7.base.test

import cats.effect.unsafe.IORuntime
import cats.effect.{IO, SyncIO}
import js7.base.log.Logger
import js7.base.test.LoggingAsyncFreeSpec.*
import js7.base.time.ScalaTime.*
import js7.base.utils.CatsUtils.syntax.logWhenItTakesLonger
import js7.base.utils.ScalaUtils.syntax.{RichJavaClass, RichThrowable}
import org.scalactic.source
import org.scalatest.freespec.AsyncFreeSpec
import org.scalatest.{Assertion, PendingStatement, Tag}
import scala.concurrent.duration.FiniteDuration
import scala.concurrent.{Future, TimeoutException}
import scala.language.implicitConversions
import scala.util.control.NonFatal
import scala.util.{Failure, Success, Try}

/**
 * Extends `AnyFreeSpec` with logging of test names and their outcomes.
 * The log lines are colored.
 * The Unix command `less -R` shows the colors on your terminal.
 **/
trait LoggingAsyncFreeSpec extends AsyncFreeSpec with TestLogging:

  protected def testTimeout: FiniteDuration = 99.s
  protected implicit def ioRuntime: IORuntime

  private val suiteName = getClass.shortClassName
  private val testAdder = new LoggingTestAdder(getClass)

  protected def suppressTestCorrelId = false

  // inline for proper source.Position
  protected implicit inline def implicitToFreeSpecStringWrapper(name: String)
    (using pos: source.Position)
  : LoggingFreeSpecStringWrapper[OwnResult, Future[Assertion], ResultOfTaggedAsInvocationOnString] =
    testAdder.toStringWrapper[OwnResult, Future[Assertion], ResultOfTaggedAsInvocationOnString](
      name,
      toUnified(convertToFreeSpecStringWrapper(name)),
      (ctx, testBody) =>
        run(name):
          for
            _ <- IO(ctx.beforeTest())
            either <- executeTest(testBody)
              .timeoutTo(testTimeout, IO.raiseError(new TimeoutException(
                s"Test timed out after ${testTimeout.pretty}: $suiteName · $name")))
              .logWhenItTakesLonger(s"$suiteName \"$name\"")
              .attempt
            _ <- IO(ctx.afterTest(either.toTry))
            result <- IO.fromEither(either)
          yield result,
      suppressCorrelId = suppressTestCorrelId)

  private def run(name: String)(io: IO[Assertion]): Assertion =
    io.unsafeRunSync()
    // It is always asynchronous due to timeout and logWhenItTakesLonger:
    //io.syncStep(Int.MaxValue)
    //  .unsafeRunSync() match
    //    case Left(io) =>
    //      //logger.trace(s"Asynchronous boundary in $suiteName · $name")
    //      io.unsafeRunSync()
    //    case Right(assertion) => assertion

  private def toUnified(stringWrapper: FreeSpecStringWrapper) =
    new LoggingFreeSpecStringWrapper.UnifiedStringWrapper[
      Future[Assertion],
      ResultOfTaggedAsInvocationOnString
    ]:
      def -(addTests: => Unit) =
        stringWrapper - addTests

      def in(testBody: => Future[Assertion]) =
        stringWrapper in testBody

      def ignore(testBody: => Future[Assertion]) =
        stringWrapper ignore testBody

      def taggedAs(tag: Tag, more: Tag*): LoggingFreeSpecStringWrapper.TaggedAs[Future[Assertion]] =
        new LoggingFreeSpecStringWrapper.TaggedAs[Future[Assertion]]:
          def in(testBody: => Future[Assertion]) =
            testBody

          def ignore(testBody: => Future[Assertion]) =
            Future.failed(new RuntimeException("Test ignored"))  // Not expected to be thrown

          def is(pending: => PendingStatement) =
            pending

  private def executeTest(testBody: => OwnResult): IO[Assertion] =
    Try(testBody) match
      case Failure(t) =>
        IO.raiseError(appendStackTrace(t))

      case Success(o: Assertion) =>
        IO.pure(o)

      case Success(o: Future[Assertion @unchecked]) =>
        IO.fromFuture(IO.pure(o))
          .handleErrorWith(t => IO.raiseError(appendStackTrace(t)))

      case Success(io: IO[Assertion]) =>
        io.handleErrorWith(t => IO.raiseError(appendStackTrace(t)))

      case Success(o: SyncIO[Assertion]) =>
        o.to[IO]
          .handleErrorWith(t => IO.raiseError(appendStackTrace(t)))


object LoggingAsyncFreeSpec:
  private type OwnResult = Assertion | Future[Assertion] | IO[Assertion] | SyncIO[Assertion]
  private lazy val logger = Logger[this.type]

  private[test] def isAsyncResult(result: Any) =
    result match
      case _: Future[?] | _: IO[?] | _: SyncIO[?] => true
      case _ => false

  private def appendStackTrace(throwable: Throwable): Throwable =
    if throwable.getClass.getName startsWith "org.scalatest." then
      throwable
    else
      throwable match
        case NonFatal(t) =>
          // Add our stacktrace
          new RuntimeException(t.toStringWithCauses, t)
        case t => t
