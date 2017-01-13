package com.sos.scheduler.engine.common.event.collector

import com.sos.scheduler.engine.common.event.EventIdGenerator
import com.sos.scheduler.engine.common.event.collector.EventCollectorTest._
import com.sos.scheduler.engine.common.scalautil.Futures.implicits.SuccessFuture
import com.sos.scheduler.engine.common.time.ScalaTime._
import com.sos.scheduler.engine.common.time.timer.TimerService
import com.sos.scheduler.engine.data.event.{AnyKeyedEvent, Event, EventId, EventRequest, EventSeq, KeyedEvent}
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{BeforeAndAfterAll, FreeSpec}
import scala.concurrent.ExecutionContext
import scala.concurrent.ExecutionContext.Implicits.global
import scala.reflect.ClassTag

/**
  * @author Joacim Zschimmer
  */
@RunWith(classOf[JUnitRunner])
final class EventCollectorTest extends FreeSpec with BeforeAndAfterAll {

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
    eventCollector.putEvent_(KeyedEvent(A1)("1"))
    eventCollector.putEvent_(KeyedEvent(A1)("2"))
    val snapshots = keyedEventQueue.after(after = EventId.BeforeFirst).get.toVector
    assert((snapshots map { _.value }) == Vector(KeyedEvent(A1)("1"), KeyedEvent(A1)("2")))
    assert((keyedEventQueue.after(after = snapshots(0).eventId).get.toVector map { _.value }) == Vector(KeyedEvent(A1)("2")))
    assert((keyedEventQueue.after(after = snapshots(1).eventId).get.toVector map { _.value }).isEmpty)
  }

  "eventCollector.when with torn event stream" in {
    val eventCollector = new MyEventCollector(EventCollector.Configuration.ForTest.copy(queueSize = 2))
    val anyFuture = eventCollector.when(EventRequest.singleClass[Event](after = EventId.BeforeFirst, 30.s))
    val bFuture = eventCollector.when(EventRequest.singleClass[BEvent](after = EventId.BeforeFirst, 30.s))
    assert(!anyFuture.isCompleted)
    eventCollector.putEvent_(KeyedEvent(A1)("1"))
    val EventSeq.NonEmpty(anyEvents) = anyFuture await 100.ms
    assert((anyEvents.toList map { _.value }) == List(KeyedEvent(A1)("1")))

    assert(!bFuture.isCompleted)
    eventCollector.putEvent_(KeyedEvent(B1)("2"))
    val EventSeq.NonEmpty(bEventsIterator) = bFuture await 100.ms
    val bEvents = bEventsIterator.toVector
    assert((bEvents map { _.value }) == Vector(KeyedEvent(B1)("2")))

    // Third event, overflowing the queue
    eventCollector.putEvent_(KeyedEvent(B1)("2"))

    val EventSeq.NonEmpty(cEventIterator) = eventCollector.when(EventRequest.singleClass[BEvent](after = bEvents.last.eventId, 1.s)) await 100.ms
    assert((cEventIterator.toList map { _.value }) == List(KeyedEvent(B1)("2")))

    assert((eventCollector.when(EventRequest.singleClass[BEvent](after = EventId.BeforeFirst, 1.s)) await 100.ms) == EventSeq.Torn)
  }

  "eventCollector.whenForKey" in {
    val eventCollector = new MyEventCollector
    eventCollector.putEvent_(KeyedEvent(A1)("1"))
    eventCollector.putEvent_(KeyedEvent(B1)("1"))
    eventCollector.putEvent_(KeyedEvent(A2)("1"))
    eventCollector.putEvent_(KeyedEvent(A2)("2"))
    eventCollector.putEvent_(KeyedEvent(B2)("1"))
    def eventsForKey[E <: Event: ClassTag](key: E#Key) = {
      val EventSeq.NonEmpty(eventIterator) = eventCollector.whenForKey[E](EventRequest.singleClass(after = EventId.BeforeFirst, 20.s), key) await 10.s
      eventIterator.toVector map { _.value }
    }
    assert(eventsForKey[AEvent]("1") == Vector(A1, A2))
    assert(eventsForKey[AEvent]("2") == Vector(A2))
    assert(eventsForKey[BEvent]("1") == Vector(B1, B2))
  }
}

private object EventCollectorTest {

  private class MyEventCollector(configuration: EventCollector.Configuration = EventCollector.Configuration.ForTest)
    (implicit
      protected val eventIdGenerator: EventIdGenerator,
      protected val timerService: TimerService,
      protected val executionContext: ExecutionContext)
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
