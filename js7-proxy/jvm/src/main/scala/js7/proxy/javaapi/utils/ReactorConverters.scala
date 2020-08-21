package js7.proxy.javaapi.utils

import monix.execution.Scheduler
import monix.reactive.Observable
import reactor.core.publisher.Flux

object ReactorConverters
{
  implicit final class FluxObservable[A](private val underlying: Observable[A]) extends AnyVal
  {
    /** Convert this Monix Observable to a Reactor Flux. */
    def asFlux(implicit scheduler: Scheduler): Flux[A] =
      Flux.from(underlying.toReactivePublisher(scheduler))
  }

  implicit final class ObservableFlux[A](private val underlying: Flux[A]) extends AnyVal
  {
    /** Convert this Reactor Flux to a Monix Observable. */
    def asObservable: Observable[A] =
      Observable.fromReactivePublisher(underlying)
  }
}
