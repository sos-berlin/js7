package com.sos.jobscheduler.common.scalautil

import com.sos.jobscheduler.base.time.Timestamp
import com.sos.jobscheduler.base.utils.CloseableIterator
import com.sos.jobscheduler.common.scalautil.Futures.implicits._
import monix.eval.Task
import monix.execution.{Cancelable, Scheduler}
import monix.reactive.Observable
import scala.collection.generic.CanBuildFrom
import scala.concurrent.duration.Duration
import scala.language.higherKinds
import scala.util.{Failure, Success, Try}

/**
  * @author Joacim Zschimmer
  */
object MonixUtils
{
  private val logger = Logger(getClass)

  object ops {
    implicit class RichTask[A](private val underlying: Task[A]) extends AnyVal {

      def await(duration: java.time.Duration)(implicit s: Scheduler): A =
        underlying.runAsync await duration

      def await(duration: Option[java.time.Duration])(implicit s: Scheduler): A =
        underlying.runAsync await duration

      def await(duration: Duration)(implicit s: Scheduler): A =
        underlying.runAsync await duration
    }

    implicit final class RichTaskTraversable[A, M[X] <: TraversableOnce[X]](private val underlying: M[Task[A]]) extends AnyVal {
      /**
        * Awaits the futures completion for the duration or infinite.
        */
      def await(duration: Option[java.time.Duration])(implicit s: Scheduler, cbf: CanBuildFrom[M[Task[A]], A, M[A]]): M[A] =
        duration match {
          case Some(o) ⇒ await(o)
          case None ⇒ awaitInfinite
        }

      def await(duration: java.time.Duration)(implicit s: Scheduler, cbf: CanBuildFrom[M[Task[A]], A, M[A]]): M[A] =
        Task.sequence(underlying)(cbf).runAsync await duration

      def awaitInfinite(implicit s: Scheduler, cbf: CanBuildFrom[M[Task[A]], A, M[A]]): M[A] =
        Task.sequence(underlying)(cbf).runAsync.awaitInfinite
    }

    implicit final class RichScheduler(private val underlying: Scheduler) extends AnyVal
    {
      def scheduleFor(timestamp: Timestamp)(action: ⇒ Unit) = {
        val nw = Timestamp.now
        Try(if (timestamp <= nw) Duration.Zero else timestamp - nw) match {
          case Success(delay) ⇒ underlying.scheduleOnce(delay)(action)
          case Failure(_) ⇒ Cancelable.empty  // More than 292 years
        }
      }
    }
  }

  def closeableIteratorToObservable[A](iterator: CloseableIterator[A]): Observable[A] = {
    logger.trace(s"closeableIteratorToObservable($iterator)")
    Observable.fromIterator(iterator.closeAtEnd)
      .doAfterTerminate { _ ⇒
        logger.trace(s"Close $iterator")
        iterator.close()
      }
      .doOnSubscriptionCancel { () ⇒
        logger.debug(s"Observable canceled. Close $iterator")
        iterator.close()
      }
  }
}
