package com.sos.jobscheduler.base.monixutils

import cats.effect.Resource
import com.sos.jobscheduler.base.problem.Checked
import com.sos.jobscheduler.base.time.Timestamp
import MonixDeadline.syntax._
import com.sos.jobscheduler.base.utils.CloseableIterator
import monix.eval.Task
import monix.execution.{Cancelable, Scheduler}
import monix.execution.cancelables.MultiAssignCancelable
import monix.reactive.Observable
import scala.concurrent.duration._
import scala.concurrent.{Future, TimeoutException}
import scala.util.{Failure, Success, Try}

object MonixBase
{
  private val FalseTask = Task.pure(false)
  private val TrueTask = Task.pure(true)

  object syntax
  {
    implicit class RichTaskCompanion(private val underlying: Task.type) extends AnyVal {
      def False = FalseTask
      def True = TrueTask
    }

    implicit class RichMonixTask[A](private val underlying: Task[A]) extends AnyVal
    {
      def maybeTimeout(duration: Duration): Task[A] =
        duration match {
          case d: FiniteDuration => underlying.timeout(d)
          case _ => underlying
        }

      def orTimeout(timeout: Duration, onTimeout: => Task[A]): Task[A] =
        timeout match {
          case d: FiniteDuration =>
            underlying.timeout(d)
              .onErrorRecoverWith { case _: TimeoutException =>
                onTimeout
              }
          case _ => underlying
        }
    }

    implicit class RichMonixObservable[A](private val underlying: Observable[A]) extends AnyVal
    {
      // Copied from Monix echoRepeated
      /** Mirror the source observable as long as the source keeps emitting
        * items, otherwise if `timeout` passes without the source emitting
        * anything new then the observable will start emitting
        * `intersperseValue` repeatedly.
        * Different from `echoRepeated`, this inserts hearbeats from start
        *
        * Note: If the source Observable keeps emitting items more
        * frequently than the length of the time window then the resulting
        * observable will mirror the source exactly.
        *
        * @param timeout the window of silence that must pass in order for the
        *        observable to start echoing the last item
        */
      final def insertHeartbeatsOnSlowUpstream(timeout: FiniteDuration, heartbeatValue: A): Observable[A] =
        new InsertHeartbeatsOnSlowUpstream[A](
          heartbeatValue +: underlying,   // Insert heartbeats from start
          timeout, onlyOnce = false, heartbeatValue
        ).drop(1)  // Remove inserted initial heartbeat
    }

    implicit class RichCheckedTask[A](private val underlying: Task[Checked[A]]) extends AnyVal
    {
      /** Converts a failed Task into a `Task[Left[Throwable]]`. */
      def materializeIntoChecked: Task[Checked[A]] =
        underlying.materialize.map(Checked.flattenTryChecked)
    }

    implicit final class RichScheduler(private val underlying: Scheduler) extends AnyVal
    {
      def scheduleFor(timestamp: Timestamp)(action: => Unit): Cancelable = {
        val nw = Timestamp.ofEpochMilli(underlying.clockRealTime(MILLISECONDS))
        Try(if (timestamp <= nw) Duration.Zero else timestamp - nw) match {
          case Success(delay) => underlying.scheduleOnce(delay)(action)
          case Failure(_) => Cancelable.empty  // More than 292 years
        }
      }

      def scheduleAtFixedRates(durations: IterableOnce[FiniteDuration])(body: => Unit): Cancelable = {
        val cancelable = MultiAssignCancelable()
        val iterator = durations.iterator
        def loop(last: MonixDeadline): Unit = {
          val nextDuration = iterator.next()
          val next = last + nextDuration
          val delay = next - underlying.now
          cancelable := (
            if (iterator.hasNext)
              underlying.scheduleOnce(delay) {
                body
                loop(next)
              }
            else
              underlying.scheduleAtFixedRate(delay, nextDuration)(body))
        }
        if (iterator.hasNext) {
          loop(underlying.now)
        }
        cancelable
      }
    }
  }

  //def deferFutureAndLog[A](f: => Future[A])(implicit A: TypeTag[A]): Task[A] =
  //  deferFutureAndLog(s"Future[${A.tpe.toString}]", f)

  def deferFutureAndLog[A](name: => String, f: => Future[A]): Task[A] =
    Task.deferFutureAction { implicit s =>
      val future = f
      if (future.isCompleted)
        future
      else {
        scribe.debug(s"Waiting for Future '$name' ...")
        future.transform { o =>
          scribe.debug(s"Future $name completed")
          o
        }
      }
    }

  def autoCloseableToObservable[A <: AutoCloseable](newA: => A): Observable[A] =
    Observable.fromResource(Resource.fromAutoCloseable(Task(newA)))

  def closeableIteratorToObservable[A](iterator: CloseableIterator[A]): Observable[A] =
    closingIteratorToObservable(iterator.closeAtEnd)

  private def closingIteratorToObservable[A](iterator: CloseableIterator[A]): Observable[A] = {
    //scribe.trace(s"closeableIteratorToObservable($iterator)")
    Observable.fromIterator(Task(iterator))
      .guaranteeCase { exitCase =>
        Task {
          scribe.trace(s"Close $iterator $exitCase")
          iterator.close()
        }
      }
  }

  /** Like Observable tailRecM, but limits the memory leak.
    * After a number of `Left` retured by `f`, the returned `Observable` is truncated.
    *
    * @see see Monix 3.2.1, https://github.com/monix/monix/issues/791
    */
  def memoryLeakLimitedObservableTailRecM[A, B](a: A, limit: Int)(f: A => Observable[Either[A, B]]): Observable[B] =
    Observable.defer {
      var leftCounter = 0
      Observable.tailRecM(a)(a =>
        f(a).flatMap {
          case o @ Left(_) =>
            if (leftCounter >= limit) {
              scribe.debug(s"Limit Observable.tailRecM after $leftCounter× Left to reduce memory leakage")
              Observable.empty
            } else {
              leftCounter += 1
              Observable.pure(o)
            }

          case o => Observable.pure(o)
        })
    }
}
