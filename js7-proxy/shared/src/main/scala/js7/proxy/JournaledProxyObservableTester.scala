package js7.proxy

import js7.base.utils.ScalaUtils._
import js7.data.event.{Event, JournaledState, KeyedEvent, Stamped}
import monix.eval.Task
import monix.execution.Scheduler
import scala.concurrent.Promise
import scala.reflect.ClassTag

object JournaledProxyObservableTester
{
  object syntax {
    implicit final class TestJournaledProxy[S <: JournaledState[S]](private val underlying: JournaledProxy[S]) extends AnyVal
    {
      def awaitEvent[E <: Event: ClassTag](
        predicate: EventAndState[E, S] => Boolean = (_: EventAndState[E, S]) => true)
        (body: Task[_])
        (implicit s: Scheduler)
      : Task[EventAndState[E, S]] =
        JournaledProxyObservableTester.this.awaitEvent(underlying, predicate)(body)
    }
  }

  private def awaitEvent[E <: Event: ClassTag, S <: JournaledState[S]](
    proxy: JournaledProxy[S],
    predicate: EventAndState[E, S] => Boolean = (_: EventAndState[E, S]) => true)
    (body: Task[_])
    (implicit s: Scheduler)
  : Task[EventAndState[E, S]] = {
    // The observing promise tries to avoid the race condition between start of observable and body.
    val observingStarted = Promise[Unit]()
    val whenAdded = proxy.observable
      .doAfterSubscribe(Task {
        observingStarted.success(())
      })
      .collect {
        case es @ EventAndState(Stamped(_, _, KeyedEvent(_, event)), _)
          if implicitClass[E] isAssignableFrom event.getClass =>
          es.asInstanceOf[EventAndState[E, S]]
      }
      .filter(predicate)
      .headL
      .runToFuture
    Task.fromFuture(observingStarted.future)
      .flatMap(_ => body)
      .flatMap(_ => Task.fromFuture(whenAdded))
      .guarantee(Task {
        whenAdded.cancel()
      })
  }
}
