package js7.tests.controller.proxy

import cats.effect.IO
import cats.effect.unsafe.IORuntime
import js7.base.fs2utils.StreamExtensions.onStart
import js7.base.monixlike.MonixLikeExtensions.{headL, unsafeToCancelableFuture}
import js7.base.thread.Futures.implicits.SuccessFuture
import js7.base.time.ScalaTime.*
import js7.base.utils.ScalaUtils.*
import js7.data.event.{Event, KeyedEvent, SnapshotableState, Stamped}
import js7.proxy.JournaledProxy
import js7.proxy.data.event.EventAndState
import scala.concurrent.Promise
import scala.reflect.ClassTag

object JournaledProxyStreamTester:

  object syntax:
    implicit final class TestJournaledProxy[S <: SnapshotableState[S]](
      private val underlying: JournaledProxy[S])
    extends AnyVal:
      def awaitEvent[E <: Event: ClassTag](
        predicate: EventAndState[E, S] => Boolean = (_: EventAndState[E, S]) => true)
        (body: IO[?])
        (using IORuntime)
      : IO[EventAndState[E, S]] =
        JournaledProxyStreamTester.this.awaitEvent(underlying, predicate)(body)

  private def awaitEvent[E <: Event: ClassTag, S <: SnapshotableState[S]](
    proxy: JournaledProxy[S],
    predicate: EventAndState[E, S] => Boolean = (_: EventAndState[E, S]) => true)
    (body: IO[?])
    (using IORuntime)
  : IO[EventAndState[E, S]] =
    // The observing promise tries to avoid the race condition between start of stream and body.
    val observingStarted = Promise[Unit]()
    val whenAdded = proxy.stream()
      .onStart(IO {
        observingStarted.success(())
      })
      .collect:
        case es @ EventAndState(Stamped(_, _, KeyedEvent(_, event)), _, _)
          if implicitClass[E].isAssignableFrom(event.getClass) =>
          es.asInstanceOf[EventAndState[E, S]]
      .filter(predicate)
      .headL
      .unsafeToCancelableFuture()
    IO.fromFuture(IO(observingStarted.future))
      .flatMap(_ => body)
      .flatMap(_ => IO.fromFuture(IO(whenAdded)))
      .guarantee(IO {
        whenAdded.cancelToFuture().await(99.s)
      })
