package com.sos.scheduler.engine.common.async

import java.time.Instant
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

  /** Future, die f als TimedCall in callQueue ausführt. */
  def callFuture[A](f: ⇒ A)(implicit callQueue: CallQueue): Future[A] =
    timedCallFuture(TimedCall.ShortTerm)(f)(callQueue)

  /** Future, die f als TimedCall in callQueue ausführt. */
  def timedCallFuture[A](at: Instant)(f: ⇒ A)(implicit callQueue: CallQueue): Future[A] = {
    val call = futureTimedCall(at)(f)
    callQueue add call
    call.future
  }

  /** TimedCall als Future, nicht in eine [[CallQueue]] eingehängt. */
  def futureCall[A](body: ⇒ A): FutureCall[A] =
    futureTimedCall(TimedCall.ShortTerm)(body)

  /** TimedCall als Future, nicht in eine [[CallQueue]] eingehängt. */
  def futureTimedCall[A](at: Instant)(body: ⇒ A): FutureCall[A] =
    functionToFutureTimedCall[A](at, () ⇒ body)

  def functionToFutureTimedCall[A](at: Instant, function: () ⇒ A): FutureCall[A] =
    new TimedCall[A] with FutureCompletion[A] {
      def epochMillis = at.toEpochMilli

      def call(): A = function()

      override def toStringPrefix = s"${function.getClass.getName} ${function.toString()}"
    }
}
