package com.sos.jobscheduler.common.akkahttp

import akka.NotUsed
import akka.stream.scaladsl.Source
import monix.execution.Scheduler
import monix.reactive.Observable

/**
  * @author Joacim Zschimmer
  */
object StreamingSupport
{
  implicit final class AkkaObservable[A](private val underlying: Observable[A]) extends AnyVal {
    def toAkkaSource(implicit scheduler: Scheduler): Source[A, NotUsed] =
      Source.fromPublisher(underlying.toReactivePublisher(scheduler))
  }
}

