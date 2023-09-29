package js7.data_for_java.reactor

import monix.execution.Scheduler
import monix.reactive.Observable
import reactor.core.publisher.Flux

object ReactorConverters:
  implicit final class FluxObservable[A](private val asScala: Observable[A]) extends AnyVal:
    /** Convert this Monix Observable to a Reactor Flux. */
    def asFlux(implicit scheduler: Scheduler): Flux[A] =
      Flux.from(asScala.toReactivePublisher(scheduler))

  implicit final class ObservableFlux[A](private val asScala: Flux[A]) extends AnyVal:
    /** Convert this Reactor Flux to a Monix Observable. */
    def asObservable: Observable[A] =
      Observable.fromReactivePublisher(asScala)
