package com.sos.jobscheduler.base.monixutils

import monix.eval.Task
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
}
