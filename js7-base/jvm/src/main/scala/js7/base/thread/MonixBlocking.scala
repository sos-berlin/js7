package js7.base.thread

import izumi.reflect.Tag
import js7.base.log.Logger
import js7.base.log.Logger.syntax.*
import js7.base.thread.Futures.implicits.*
import js7.base.time.ScalaTime.*
import js7.base.utils.StackTraces.StackTraceThrowable
import monix.eval.Task
import monix.execution.Scheduler
import scala.collection.BuildFrom
import scala.concurrent.duration.*
import scala.util.control.NonFatal

/**
  * @author Joacim Zschimmer
  */
object MonixBlocking
{
  private val logger = Logger[this.type]

  object syntax {
    implicit class RichTask[A](private val underlying: Task[A]) extends AnyVal
    {
      def await(duration: Duration)(implicit s: Scheduler, src: sourcecode.Enclosing): A = {
        try
          logger
            .traceTask(s"${src.value} await ${duration.pretty}")(
              underlying)
            .runSyncStep
            .fold(_.runSyncUnsafe(duration), identity)
        catch { case NonFatal(t) =>
          if (t.getStackTrace.forall(_.getClassName != getClass.getName)) {
            t.appendCurrentStackTrace
          }
          throw t
        }
      }

      def awaitInfinite(implicit s: Scheduler, src: sourcecode.Enclosing): A =
        await(Duration.Inf)
    }

    implicit final class RichTaskTraversable[A, M[X] <: Iterable[X]](private val underlying: M[Task[A]]) extends AnyVal
    {
      def await(duration: FiniteDuration)
        (implicit
          s: Scheduler,
          cbf: BuildFrom[M[Task[A]], A, M[A]], MA: Tag[M[A]])
      : M[A] =
        Task.sequence(underlying)(cbf).runToFuture await duration

      def awaitInfinite(implicit s: Scheduler, cbf: BuildFrom[M[Task[A]], A, M[A]]): M[A] =
        Task.sequence(underlying)(cbf).runToFuture.awaitInfinite
    }
  }
}
