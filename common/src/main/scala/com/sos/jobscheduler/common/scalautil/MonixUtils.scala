package com.sos.jobscheduler.common.scalautil

import cats.effect.Resource
import com.sos.jobscheduler.base.problem.Checked
import com.sos.jobscheduler.base.time.Timestamp
import com.sos.jobscheduler.base.utils.CloseableIterator
import com.sos.jobscheduler.common.scalautil.Futures.implicits._
import com.sos.jobscheduler.common.scalautil.Futures.promiseFuture
import monix.eval.Task
import monix.execution.{Cancelable, Scheduler}
import monix.reactive.Observable
import scala.collection.generic.CanBuildFrom
import scala.concurrent.Promise
import scala.concurrent.duration._
import scala.language.higherKinds
import scala.reflect.runtime.universe._
import scala.util.{Failure, Success, Try}

/**
  * @author Joacim Zschimmer
  */
object MonixUtils
{
  private val logger = Logger(getClass)
  private val FalseTask = Task.pure(false)
  private val TrueTask = Task.pure(true)

  object ops {
    implicit class RichTaskCompanion(private val underlying: Task.type) extends AnyVal {
      def False = FalseTask
      def True = TrueTask
    }

    implicit class RichTask[A](private val underlying: Task[A]) extends AnyVal
    {
      def await(duration: FiniteDuration)(implicit s: Scheduler, A: TypeTag[A]): A =
        underlying.runToFuture await duration

      def awaitInfinite(implicit s: Scheduler, A: TypeTag[A]): A =
        underlying.runToFuture.awaitInfinite
    }

    implicit class RichCheckedTask[A](private val underlying: Task[Checked[A]]) extends AnyVal
    {
      /** Converts a failed Task into a `Task[Right[Throwable]]`. */
      def materializeIntoChecked: Task[Checked[A]] =
        underlying.materialize.map(Checked.flattenTryChecked)
    }

    implicit final class RichTaskTraversable[A, M[X] <: Iterable[X]](private val underlying: M[Task[A]]) extends AnyVal
    {
      def await(duration: FiniteDuration)(implicit s: Scheduler, cbf: CanBuildFrom[M[Task[A]], A, M[A]], MA: TypeTag[M[A]]): M[A] =
        Task.sequence(underlying)(cbf).runToFuture await duration

      def awaitInfinite(implicit s: Scheduler, cbf: CanBuildFrom[M[Task[A]], A, M[A]]): M[A] =
        Task.sequence(underlying)(cbf).runToFuture.awaitInfinite
    }

    implicit final class RichScheduler(private val underlying: Scheduler) extends AnyVal
    {
      def scheduleFor(timestamp: Timestamp)(action: => Unit): Cancelable = {
        val nw = Timestamp.now
        Try(if (timestamp <= nw) Duration.Zero else timestamp - nw) match {
          case Success(delay) => underlying.scheduleOnce(delay)(action)
          case Failure(_) => Cancelable.empty  // More than 292 years
        }
      }
    }
  }

  def promiseTask[A](body: Promise[A] => Unit): Task[A] =
    Task.deferFuture(promiseFuture(body))

  def autoCloseableToObservable[A <: AutoCloseable](newA: => A): Observable[A] =
    Observable.fromResource(Resource.fromAutoCloseable(Task(newA)))

  def closeableIteratorToObservable[A](iterator: CloseableIterator[A]): Observable[A] =
    closingIteratorToObservable(iterator.closeAtEnd)

  private def closingIteratorToObservable[A](iterator: CloseableIterator[A]): Observable[A] = {
    logger.trace(s"closeableIteratorToObservable($iterator)")
    Observable.fromIterator(Task(iterator))
      .guaranteeCase { exitCase =>
        Task {
          logger.trace(s"Close $iterator $exitCase")
          iterator.close()
        }
      }
  }
}
