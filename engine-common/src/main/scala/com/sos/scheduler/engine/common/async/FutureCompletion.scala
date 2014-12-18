package com.sos.scheduler.engine.common.async

import org.joda.time.Instant
import scala.concurrent.{Future, Promise}
import scala.util.Try

trait FutureCompletion[A] {
  this: TimedCall[A] =>

  private val promise = Promise[A]()

  final override protected def onComplete(result: Try[A]): Unit = {
    promise complete result
  }

  final def future = promise.future
}


object FutureCompletion {
  type FutureCall[A] = TimedCall[A] with FutureCompletion[A]

  /** Future, der f als TimedCall in callQueue ausführt. */
  def callFuture[A](f: => A)(implicit callQueue: CallQueue): Future[A] =
    timedCallFuture(TimedCall.shortTerm)(f)(callQueue)

  /** Future, der f als TimedCall in callQueue ausführt. */
  def timedCallFuture[A](at: Instant)(f: => A)(implicit callQueue: CallQueue): Future[A] = {
    val call = futureTimedCall(at)(f)
    callQueue add call
    call.future
  }

  /** TimedCall als Future, nicht in eine [[CallQueue]] eingehängt. */
  def futureCall[A](f: => A): FutureCall[A] =
    futureTimedCall(TimedCall.shortTerm)(f)

  /** TimedCall als Future, nicht in eine [[CallQueue]] eingehängt. */
  def futureTimedCall[A](at: Instant)(f: => A): FutureCall[A] =
    new TimedCall[A] with FutureCompletion[A] {
      def epochMillis = at.getMillis
      def call(): A = f
    }
}
