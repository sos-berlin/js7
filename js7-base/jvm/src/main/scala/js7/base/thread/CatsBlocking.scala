package js7.base.thread

import cats.Traverse
import cats.effect.IO
import cats.effect.unsafe.IORuntime
import cats.syntax.traverse.*
import izumi.reflect.Tag
import js7.base.thread.Futures.implicits.*
import js7.base.thread.Futures.makeBlockingWaitingString
import js7.base.time.ScalaTime.*
import js7.base.utils.CatsUtils.syntax.logWhenItTakesLonger
import js7.base.utils.ScalaUtils.syntax.RichAny
import js7.base.utils.StackTraces.StackTraceThrowable
import scala.concurrent.TimeoutException
import scala.concurrent.duration.*
import scala.util.control.NonFatal

/**
  * @author Joacim Zschimmer
  */
object CatsBlocking:

  object syntax:
    extension [A](io: IO[A])
      def await(duration: Duration)
        (using A: Tag[A], rt: IORuntime,
          src: sourcecode.Enclosing, file: sourcecode.FileName, line: sourcecode.Line)
      : A =
        inline def name = makeBlockingWaitingString[A]("IO", duration)

        try
          io
            .pipeIf(duration != Duration.Inf):
              _.timeoutTo(
                duration,
                IO.defer(IO.raiseError(throw new TimeoutException(name + " timed out"))))
            .syncStep(Int.MaxValue)
            .unsafeRunSync()
            .match
              case Left(io) => io.logWhenItTakesLonger(name).unsafeRunSync()
              case Right(a) => a
        catch case NonFatal(t) =>
          if t.getStackTrace.forall(_.getClassName != getClass.getName) then
            t.appendCurrentStackTrace
          throw t

      def awaitInfinite(using A: Tag[A], rt: IORuntime, src: sourcecode.Enclosing,
        file: sourcecode.FileName, line: sourcecode.Line)
      : A =
        await(Duration.Inf)

    extension [F[_], A](iterable: F[IO[A]])
      def await(duration: FiniteDuration)
        (using rt: IORuntime, t: Traverse[F], fTag: Tag[F], A: Tag[A],
          enc: sourcecode.Enclosing, file: sourcecode.FileName, line: sourcecode.Line)
      : F[A] =
        inline def name = makeBlockingWaitingString(fTag.tag.toString, duration)
        iterable
          .sequence
          .timeoutTo(duration,
            IO.raiseError(new TimeoutException(name + " timed out")))
          .logWhenItTakesLonger(name)
          .unsafeRunSync()

      def awaitInfinite(using IORuntime, Traverse[F], Tag[F[A]],
        sourcecode.Enclosing, sourcecode.FileName, sourcecode.Line)
      : F[A] =
        iterable.sequence.unsafeToFuture().awaitInfinite
