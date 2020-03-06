package com.sos.jobscheduler.base.monixutils

import monix.eval.Task
import monix.reactive.Observable
import scala.concurrent.TimeoutException
import scala.concurrent.duration.{Duration, FiniteDuration}

object MonixBase
{
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
    final def beatOnSlowUpstream(timeout: FiniteDuration, heartbeatValue: A): Observable[A] =
      new BeatOnSlowUpstream[A](
        heartbeatValue +: underlying,   // Insert heartbeats from start
        timeout, onlyOnce = false, heartbeatValue
      ).drop(1)  // Remove inserted initial heartbeat
  }
}
