package js7.data_for_java.reactor

import cats.effect.unsafe.IORuntime
import cats.effect.IO
import fs2.Stream
import fs2.interop.reactivestreams.*
import reactor.core.publisher.Flux

object ReactorConverters:

  extension[A](stream: Stream[IO, A])

    /** Convert this FS2 Stream to a Reactor Flux. */
    def asFlux(using IORuntime): Flux[A] =
      stream.toUnicastPublisher
        .use: publisher =>
          IO(Flux.from(publisher))
        .unsafeRunSync()


  extension [A](flux: Flux[A])

    /** Convert this Reactor Flux to a FS2 Stream. */
    def asStream: Stream[IO, A] =
      fromPublisher(flux, bufferSize = 1)
