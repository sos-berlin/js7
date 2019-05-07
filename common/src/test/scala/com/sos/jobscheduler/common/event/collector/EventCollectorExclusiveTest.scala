package com.sos.jobscheduler.common.event.collector

import com.sos.jobscheduler.common.event.EventIdGenerator
import com.sos.jobscheduler.common.event.collector.EventCollectorExclusiveTest._
import com.sos.jobscheduler.common.scalautil.Futures.implicits.SuccessFuture
import com.sos.jobscheduler.common.scalautil.MonixUtils.ops._
import com.sos.jobscheduler.base.time.ScalaTime._
import com.sos.jobscheduler.data.event.{AnyKeyedEvent, Event, EventId, EventRequest, EventSeq, TearableEventSeq}
import monix.execution.Scheduler.Implicits.global
import org.scalatest.{BeforeAndAfterAll, FreeSpec}
import scala.concurrent.duration._
import scala.reflect.ClassTag

/**
  * @author Joacim Zschimmer
  */
final class EventCollectorExclusiveTest extends FreeSpec with BeforeAndAfterAll {

  private implicit val eventIdGenerator = new EventIdGenerator

  "eventCollector.keyedEventQueue.after" in {
    val eventCollector = new MyEventCollector
    import eventCollector.keyedEventQueue
    assert(keyedEventQueue.after(after = EventId.BeforeFirst).get.isEmpty)
    eventCollector.putEvent_("1" <-: A1)
    eventCollector.putEvent_("2" <-: A1)
    val stampedEventSeq = keyedEventQueue.after(after = EventId.BeforeFirst).get.toVector
    assert((stampedEventSeq map { _.value }) == Vector("1" <-: A1, "2" <-: A1))
    assert((keyedEventQueue.after(after = stampedEventSeq(0).eventId).get.toVector map { _.value }) == Vector("2" <-: A1))
    assert((keyedEventQueue.after(after = stampedEventSeq(1).eventId).get.toVector map { _.value }).isEmpty)
  }

  "eventCollector.when with torn event stream" in {
    val eventCollector = new MyEventCollector(EventCollector.Configuration.ForTest.copy(queueSize = 2))
    val anyFuture = eventCollector.when(EventRequest.singleClass[Event](timeout = Some(30.s))).runToFuture
    val bFuture = eventCollector.when(EventRequest.singleClass[BEvent](timeout = Some(30.s))).runToFuture
    assert(!anyFuture.isCompleted)
    eventCollector.putEvent_("1" <-: A1)
    val EventSeq.NonEmpty(anyEvents) = anyFuture await 9.s
    assert((anyEvents.toList map { _.value }) == List("1" <-: A1))

    assert(!bFuture.isCompleted)
    eventCollector.putEvent_("2" <-: B1)
    val EventSeq.NonEmpty(bEventsIterator) = bFuture await 9.s
    val bEvents = bEventsIterator.toVector
    assert((bEvents map { _.value }) == Vector("2" <-: B1))

    // Third event, overflowing the queue
    eventCollector.putEvent_("2" <-: B1)

    val EventSeq.NonEmpty(cEventIterator) = eventCollector.when(EventRequest.singleClass[BEvent](after = bEvents.last.eventId, Some(1.second))) await 100.ms
    assert((cEventIterator.toList map { _.value }) == List("2" <-: B1))

    assert((eventCollector.when(EventRequest.singleClass[BEvent](timeout = Some(1.second))) await 500.ms).isInstanceOf[TearableEventSeq.Torn])
  }

  "eventCollector.whenKey, whenKeyedEvent" in {
    val eventCollector = new MyEventCollector
    eventCollector.putEvent_("1" <-: A1)
    eventCollector.putEvent_("1" <-: B1)
    eventCollector.putEvent_("1" <-: A2)
    eventCollector.putEvent_("2" <-: A2)
    eventCollector.putEvent_("1" <-: B2)

    def eventsForKey[E <: Event: ClassTag](key: E#Key) = {
      val EventSeq.NonEmpty(eventIterator) = eventCollector.whenKey[E](EventRequest.singleClass(timeout = Some(20.s)), key) await 10.s
      eventIterator.toVector map { _.value }
    }
    assert(eventsForKey[AEvent]("1") == Vector(A1, A2))
    assert(eventsForKey[AEvent]("2") == Vector(A2))
    assert(eventsForKey[BEvent]("1") == Vector(B1, B2))

    def keyedEvent[E <: Event: ClassTag](key: E#Key) =
      eventCollector.whenKeyedEvent[E](EventRequest.singleClass(timeout = Some(20.s)), key) await 10.s
    assert(keyedEvent[AEvent]("1") == A1)
    assert(keyedEvent[AEvent]("2") == A2)
    assert(keyedEvent[BEvent]("1") == B1)
  }
}

private object EventCollectorExclusiveTest
{
  private class MyEventCollector(configuration: EventCollector.Configuration = EventCollector.Configuration.ForTest)
    (implicit protected val eventIdGenerator: EventIdGenerator)
  extends EventCollector(configuration)
  with EventIdGenerating
  {
    def putEvent_(keyedEvent: AnyKeyedEvent) = putEvent(keyedEvent)
  }

  private trait AEvent extends Event {
    type Key = String
  }

  private case object A1 extends AEvent
  private case object A2 extends AEvent

  private trait BEvent extends Event {
    type Key = String
  }

  private case object B1 extends BEvent
  private case object B2 extends BEvent
}
