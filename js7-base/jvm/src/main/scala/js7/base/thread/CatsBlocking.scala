package js7.base.thread

import cats.Traverse
import cats.effect.IO
import cats.effect.unsafe.IORuntime
import cats.syntax.traverse.*
import izumi.reflect.Tag
import js7.base.log.Logger
import js7.base.log.Logger.syntax.*
import js7.base.thread.Futures.implicits.*
import js7.base.time.ScalaTime.*
import js7.base.utils.StackTraces.StackTraceThrowable
import scala.collection.BuildFrom
import scala.concurrent.TimeoutException
import scala.concurrent.duration.*
import scala.util.control.NonFatal

/**
  * @author Joacim Zschimmer
  */
object CatsBlocking:
  private val logger = Logger[this.type]

  object syntax:
    extension [A](io: IO[A])
      def await(duration: Duration)(using rt: IORuntime, src: sourcecode.Enclosing): A =
        try
          logger
            .traceIO(s"${src.value} await ${duration.pretty}"):
              io
            .unsafeRunSync()
        catch case NonFatal(t) =>
          if t.getStackTrace.forall(_.getClassName != getClass.getName) then
            t.appendCurrentStackTrace
          throw t

      def awaitInfinite(implicit rt: IORuntime, src: sourcecode.Enclosing): A =
        await(Duration.Inf)

    extension [A, M[X] <: Iterable[X]](io: M[IO[A]])
      def await(duration: FiniteDuration)
        (using rt: IORuntime, M: Traverse[M], tag: Tag[M[A]], src: sourcecode.Enclosing)
      : M[A] =
        io.sequence.unsafeRunTimed(duration).getOrElse(
          throw new TimeoutException(s"${src.value}.await(${duration.pretty}) timed out"))

      def awaitInfinite(using IORuntime, Traverse[M]): M[A] =
        io.sequence.unsafeToFuture().awaitInfinite
