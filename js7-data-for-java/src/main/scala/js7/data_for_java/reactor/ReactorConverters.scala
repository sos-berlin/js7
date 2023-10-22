package js7.data_for_java.reactor

import monix.execution.Scheduler
import fs2.Stream
import reactor.core.publisher.Flux

object ReactorConverters:

  implicit final class FluxStream[A](private val asScala: Stream[IO, A]) extends AnyVal:
    /** Convert this Monix Stream to a Reactor Flux. */
    def asFlux(implicit scheduler: Scheduler): Flux[A] =
      Flux.from(asScala.toReactivePublisher(scheduler))

  implicit final class StreamFlux[A](private val asScala: Flux[A]) extends AnyVal:
    /** Convert this Reactor Flux to a Monix Stream. */
    def asStream: Stream[IO, A] =
      Stream.fromReactivePublisher(asScala)
