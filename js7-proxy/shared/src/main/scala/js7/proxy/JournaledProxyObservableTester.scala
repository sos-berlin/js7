package js7.proxy

import js7.base.time.ScalaTime._
import js7.base.utils.ScalaUtils._
import js7.common.scalautil.Futures.implicits.SuccessFuture
import js7.data.event.{Event, JournaledState, KeyedEvent, Stamped}
import monix.execution.Scheduler
import scala.concurrent.duration.FiniteDuration
import scala.reflect.ClassTag

object JournaledProxyObservableTester
{
  private val DefaultTimeout = 99.s

  object syntax {
    implicit final class TestJournaledProxy[S <: JournaledState[S]](private val underlying: JournaledProxy[S]) extends AnyVal
    {
      def awaitEvent[E <: Event: ClassTag](
        predicate: EventAndState[E, S] => Boolean = (_: EventAndState[E, S]) => true,
        timeout: FiniteDuration = DefaultTimeout)
        (body: => Unit)
        (implicit s: Scheduler)
      : EventAndState[E, S] =
        JournaledProxyObservableTester.this.awaitEvent(underlying, predicate, timeout)(body)
    }
  }

  private def awaitEvent[E <: Event: ClassTag, S <: JournaledState[S]](
    proxy: JournaledProxy[S],
    predicate: EventAndState[E, S] => Boolean = (_: EventAndState[E, S]) => true,
    timeout: FiniteDuration = DefaultTimeout)
    (body: => Unit)
    (implicit s: Scheduler)
  : EventAndState[E, S] = {
    val whenAdded = proxy.observable
      .map { o => scribe.info(s"### ${o.stampedEvent}"); o }
      .collect {
        case es @ EventAndState(Stamped(_, _, KeyedEvent(_, event)), _)
          if implicitClass[E] isAssignableFrom event.getClass =>
          es.asInstanceOf[EventAndState[E, S]]
      }
      .filter(predicate)
      .headL
      .runToFuture
    try {
      body
      whenAdded.await(timeout)
    } finally whenAdded.cancel()
  }
}
