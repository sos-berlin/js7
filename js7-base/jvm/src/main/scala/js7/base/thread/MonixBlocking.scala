package js7.base.thread

import js7.base.thread.Futures.implicits._
import monix.eval.Task
import monix.execution.Scheduler
import scala.collection.BuildFrom
import scala.concurrent.duration._
import scala.reflect.runtime.universe._

/**
  * @author Joacim Zschimmer
  */
object MonixBlocking
{
  object syntax {
    implicit class RichTask[A](private val underlying: Task[A]) extends AnyVal
    {
      def await(duration: FiniteDuration)(implicit s: Scheduler): A =
        underlying
          .runSyncStep
          .fold(_.runSyncUnsafe(duration), identity)

      def awaitInfinite(implicit s: Scheduler): A =
        underlying.runToFuture.awaitInfinite
    }

    implicit final class RichTaskTraversable[A, M[X] <: Iterable[X]](private val underlying: M[Task[A]]) extends AnyVal
    {
      def await(duration: FiniteDuration)(implicit s: Scheduler, cbf: BuildFrom[M[Task[A]], A, M[A]], MA: WeakTypeTag[M[A]]): M[A] =
        Task.sequence(underlying)(cbf).runToFuture await duration

      def awaitInfinite(implicit s: Scheduler, cbf: BuildFrom[M[Task[A]], A, M[A]]): M[A] =
        Task.sequence(underlying)(cbf).runToFuture.awaitInfinite
    }
  }
}
