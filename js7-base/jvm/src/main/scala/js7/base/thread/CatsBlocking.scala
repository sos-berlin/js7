package js7.base.thread

import cats.Traverse
import cats.effect.IO
import cats.effect.unsafe.IORuntime
import cats.syntax.traverse.*
import izumi.reflect.Tag
import java.util.concurrent.ArrayBlockingQueue
import js7.base.catsutils.CatsEffectExtensions.timeoutAndFail
import js7.base.thread.Futures.implicits.*
import js7.base.thread.Futures.makeBlockingWaitingString
import js7.base.utils.CatsUtils.syntax.logWhenItTakesLonger
import js7.base.utils.ScalaUtils.syntax.{RichAny, RichThrowableEither}
import js7.base.utils.StackTraces.StackTraceThrowable
import scala.concurrent.duration.*
import scala.concurrent.{TimeoutException, blocking}
import scala.util.control.NonFatal

/**
  * @author Joacim Zschimmer
  */
object CatsBlocking:

  object syntax:
    extension [A](io: IO[A])
      def await(duration: Duration, dontLog: Boolean = false)
        (using A: Tag[A], rt: IORuntime,
          src: sourcecode.Enclosing, file: sourcecode.FileName, line: sourcecode.Line)
      : A =
        inline def name = makeBlockingWaitingString[A]
        try
          io.pipeIf(duration != Duration.Inf):
            _.timeoutAndFail(duration)(new TimeoutException(name + " timed out"))
          .syncStep(Int.MaxValue)
          .unsafeRunSync() match
            case Left(io) =>
              io
                .pipeIf(!dontLog):
                  _.logWhenItTakesLonger(name)
                .unsafeRunSyncX()
            case Right(a) => a
        catch case NonFatal(t) =>
          if t.getStackTrace.forall(_.getClassName != getClass.getName) then
            t.appendCurrentStackTrace
          throw t

      def awaitInfinite(using
        Tag[A], IORuntime, sourcecode.Enclosing, sourcecode.FileName, sourcecode.Line)
      : A =
        await(Duration.Inf)

    extension [F[_], A](iterable: F[IO[A]])
      def await(duration: FiniteDuration)(using
        rt: IORuntime, t: Traverse[F], A: Tag[A], file: sourcecode.FileName, line: sourcecode.Line)
      : F[A] =
        inline def name = makeBlockingWaitingString
        iterable
          .sequence
          .timeoutAndFail(duration)(new TimeoutException(name + " timed out"))
          .logWhenItTakesLonger(name)
          .unsafeRunSyncX()

      def awaitInfinite(using IORuntime, Traverse[F], Tag[F[A]],
        sourcecode.Enclosing, sourcecode.FileName, sourcecode.Line)
      : F[A] =
        iterable.sequence.unsafeToFuture().awaitInfinite


  extension[A](io: IO[A])
    /** Like Cats Effect's unsafeRunSync, but does not catch InterruptedException. */
    @throws[InterruptedException]
    def unsafeRunSyncX()(using IORuntime): A =
      val queue = ArrayBlockingQueue[Either[Throwable, A]](1)
      io.unsafeRunAsync(queue.offer)
      blocking(queue.take()).orThrow
