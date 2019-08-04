package com.sos.jobscheduler.common.http

import akka.NotUsed
import akka.stream.Materializer
import akka.stream.scaladsl.{Sink, Source}
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

  implicit final class ObservableAkkaSource[Out, Mat](private val underlying: Source[Out, Mat]) extends AnyVal {
    def toObservable(implicit materializer: Materializer): Observable[Out] =
      Observable.fromReactivePublisher(
        underlying.runWith(Sink.asPublisher(fanout = false)))
  }
}

