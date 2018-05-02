package com.sos.jobscheduler.common.scalautil

import com.sos.jobscheduler.common.scalautil.Futures.implicits._
import java.time.Duration
import monix.eval.Task
import monix.execution.Scheduler

/**
  * @author Joacim Zschimmer
  */
object MonixUtils
{
  object ops {
    implicit class RichTask[A](private val underlying: Task[A]) extends AnyVal {
      def await(duration: Duration)(implicit s: Scheduler): A =
        underlying.runAsync await duration

      def await(duration: Option[Duration])(implicit s: Scheduler): A =
        underlying.runAsync await duration
    }
  }
}
