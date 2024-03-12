package js7.data_for_java.reactor

import cats.effect.IO
import cats.effect.unsafe.IORuntime
import fs2.Stream
import fs2.interop.reactivestreams.{PublisherOps, StreamOps}
import java.util.function.Function.identity as jIdentity
import org.reactivestreams.Publisher
import reactor.core.publisher.{Flux, Mono}

object ReactorConverters:

  extension [A](stream: Stream[IO, A])
    /** Convert this FS2 Stream to a Reactor Flux. */
    def asFlux(using IORuntime): Flux[A] =
      Mono.fromFuture:
        stream.toUnicastPublisher
          .allocated
          .map: (publisher, release) =>
            Flux.usingWhen[A, Publisher[A]](
              Mono.just(publisher),
              jIdentity,
              _ =>
                Mono.fromFuture: () =>
                  release.unsafeToCompletableFuture())
          .unsafeToCompletableFuture()
      .flux
      .flatMap(jIdentity)


  extension [A](flux: Flux[A])
    /** Convert this Reactor Flux to a FS2 Stream. */
    def asFs2Stream(bufferSize: Int = 1): Stream[IO, A] =
      flux.toStreamBuffered[IO](bufferSize = bufferSize)
