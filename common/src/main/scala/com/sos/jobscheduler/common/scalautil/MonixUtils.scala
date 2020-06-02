package com.sos.jobscheduler.common.scalautil

import com.sos.jobscheduler.common.scalautil.Futures.implicits._
import com.sos.jobscheduler.common.scalautil.Futures.promiseFuture
import monix.eval.Task
import monix.execution.Scheduler
import scala.collection.BuildFrom
import scala.concurrent.Promise
import scala.concurrent.duration._
import scala.reflect.runtime.universe._

/**
  * @author Joacim Zschimmer
  */
object MonixUtils
{
  object syntax {
    implicit class RichTask[A](private val underlying: Task[A]) extends AnyVal
    {
      def await(duration: FiniteDuration)(implicit s: Scheduler, A: TypeTag[A]): A =
        underlying.runToFuture await duration

      def awaitInfinite(implicit s: Scheduler, A: TypeTag[A]): A =
        underlying.runToFuture.awaitInfinite
    }

    implicit final class RichTaskTraversable[A, M[X] <: Iterable[X]](private val underlying: M[Task[A]]) extends AnyVal
    {
      def await(duration: FiniteDuration)(implicit s: Scheduler, cbf: BuildFrom[M[Task[A]], A, M[A]], MA: TypeTag[M[A]]): M[A] =
        Task.sequence(underlying)(cbf).runToFuture await duration

      def awaitInfinite(implicit s: Scheduler, cbf: BuildFrom[M[Task[A]], A, M[A]]): M[A] =
        Task.sequence(underlying)(cbf).runToFuture.awaitInfinite
    }
  }

  def promiseTask[A](body: Promise[A] => Unit): Task[A] =
    Task.deferFuture(promiseFuture(body))
}
