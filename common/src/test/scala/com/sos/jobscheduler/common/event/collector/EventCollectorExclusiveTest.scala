package com.sos.jobscheduler.common.event.collector

import com.sos.jobscheduler.common.event.EventIdGenerator
import com.sos.jobscheduler.common.event.collector.EventCollectorExclusiveTest._
import com.sos.jobscheduler.common.scalautil.Futures.implicits.SuccessFuture
import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.common.time.timer.TimerService
import com.sos.jobscheduler.data.event.{AnyKeyedEvent, Event, EventId, EventRequest, EventSeq, TearableEventSeq}
import org.scalatest.{BeforeAndAfterAll, FreeSpec}
import scala.concurrent.ExecutionContext
import scala.reflect.ClassTag

/**
  * @author Joacim Zschimmer
  */
final class EventCollectorExclusiveTest extends FreeSpec with BeforeAndAfterAll {

  import scala.concurrent.ExecutionContext.Implicits.global

  private implicit val eventIdGenerator = new EventIdGenerator
  private implicit lazy val timerService = TimerService()

  override protected def afterAll() = {
    timerService.close()
    super.afterAll()
  }

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
    val anyFuture = eventCollector.when(EventRequest.singleClass[Event](after = EventId.BeforeFirst, 30.s))
    val bFuture = eventCollector.when(EventRequest.singleClass[BEvent](after = EventId.BeforeFirst, 30.s))
    assert(!anyFuture.isCompleted)
    eventCollector.putEvent_("1" <-: A1)
    val EventSeq.NonEmpty(anyEvents) = anyFuture await 100.ms
    assert((anyEvents.toList map { _.value }) == List("1" <-: A1))

    assert(!bFuture.isCompleted)
    eventCollector.putEvent_("2" <-: B1)
    val EventSeq.NonEmpty(bEventsIterator) = bFuture await 100.ms
    val bEvents = bEventsIterator.toVector
    assert((bEvents map { _.value }) == Vector("2" <-: B1))

    // Third event, overflowing the queue
    eventCollector.putEvent_("2" <-: B1)

    val EventSeq.NonEmpty(cEventIterator) = eventCollector.when(EventRequest.singleClass[BEvent](after = bEvents.last.eventId, 1.s)) await 100.ms
    assert((cEventIterator.toList map { _.value }) == List("2" <-: B1))

    assert((eventCollector.when(EventRequest.singleClass[BEvent](after = EventId.BeforeFirst, 1.s)) await 500.ms).isInstanceOf[TearableEventSeq.Torn])
  }

  "eventCollector.whenForKey, whenKeyedEvent" in {
    val eventCollector = new MyEventCollector
    eventCollector.putEvent_("1" <-: A1)
    eventCollector.putEvent_("1" <-: B1)
    eventCollector.putEvent_("1" <-: A2)
    eventCollector.putEvent_("2" <-: A2)
    eventCollector.putEvent_("1" <-: B2)

    def eventsForKey[E <: Event: ClassTag](key: E#Key) = {
      val EventSeq.NonEmpty(eventIterator) = eventCollector.whenForKey[E](EventRequest.singleClass(after = EventId.BeforeFirst, 20.s), key) await 10.s
      eventIterator.toVector map { _.value }
    }
    assert(eventsForKey[AEvent]("1") == Vector(A1, A2))
    assert(eventsForKey[AEvent]("2") == Vector(A2))
    assert(eventsForKey[BEvent]("1") == Vector(B1, B2))

    def keyedEvent[E <: Event: ClassTag](key: E#Key) =
      eventCollector.whenKeyedEvent[E](EventRequest.singleClass(after = EventId.BeforeFirst, 20.s), key) await 10.s
    assert(keyedEvent[AEvent]("1") == A1)
    assert(keyedEvent[AEvent]("2") == A2)
    assert(keyedEvent[BEvent]("1") == B1)
  }
}

private object EventCollectorExclusiveTest {

  private class MyEventCollector(configuration: EventCollector.Configuration = EventCollector.Configuration.ForTest)
    (implicit
      protected val eventIdGenerator: EventIdGenerator,
      protected val timerService: TimerService,
      protected val executionContext: ExecutionContext)
  extends EventCollector(initialOldestEventId = EventId.BeforeFirst, configuration)
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
