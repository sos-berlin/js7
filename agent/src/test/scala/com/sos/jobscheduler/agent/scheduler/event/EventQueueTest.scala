package com.sos.jobscheduler.agent.scheduler.event

import akka.actor.{ActorSystem, Props}
import com.sos.jobscheduler.agent.scheduler.event.EventQueueTest._
import com.sos.jobscheduler.common.scalautil.Futures.implicits._
import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.common.time.timer.TimerService
import com.sos.jobscheduler.data.engine2.order.OrderEvent
import com.sos.jobscheduler.data.engine2.order.OrderEvent.OrderReady
import com.sos.jobscheduler.data.event.{EventId, EventSeq, KeyedEvent, Stamped}
import com.sos.jobscheduler.data.order.OrderId
import java.time.Duration
import org.scalatest.{BeforeAndAfterAll, FreeSpec}
import scala.collection.immutable.Seq
import scala.concurrent.{Future, Promise}

/**
  * @author Joacim Zschimmer
  */
final class EventQueueTest extends FreeSpec with BeforeAndAfterAll {

  private val actorSystem = ActorSystem("EventQueueTest")
  private val timerService = new TimerService(idleTimeout = Some(1.s))
  private val eventQueue = actorSystem actorOf Props { new EventQueue(timerService) }

  override def afterAll() = {
    actorSystem.terminate()
    super.afterAll()
  }

  "test" in {
    assert((requestEvents(after = EventId.BeforeFirst, timeout = 0.s) await 99.s) == EventSeq.Empty(EventId.BeforeFirst))
    assert((requestEvents(after = EventId.BeforeFirst, timeout = 10.ms) await 99.s) == EventSeq.Empty(EventId.BeforeFirst))

    val aKeyedEvent = KeyedEvent(OrderReady)(OrderId("1"))
    eventQueue ! Stamped(111, aKeyedEvent)
    val aEventSeq = (requestEvents(after = EventId.BeforeFirst, timeout = 0.s) await 99.s).asInstanceOf[MyNonEmptyEventSeq]
    assert((aEventSeq.stampeds map { _.value }) == List(aKeyedEvent))
    val aEventId = aEventSeq.stampeds.last.eventId
    assert(aEventId == 111)

    val whenBEventSeq = requestEvents(after = aEventId, timeout = 99.s).mapTo[EventSeq.NonEmpty[Seq, KeyedEvent[OrderEvent]]]
    sleep(50.ms)
    val bKeyedEvent = KeyedEvent(OrderReady)(OrderId("1"))
    eventQueue ! Stamped(222, bKeyedEvent)
    val bEventSeq = whenBEventSeq await 99.s
    assert((bEventSeq.stampeds map { _.value }) == List(bKeyedEvent))
    assert(bEventSeq.stampeds.last.eventId > EventId.BeforeFirst)

    val cEventSeq = (requestEvents(after = EventId.BeforeFirst, timeout = 0.s) await 99.s).asInstanceOf[MyNonEmptyEventSeq]
    assert((cEventSeq.stampeds map { _.value }) == List(aKeyedEvent, bKeyedEvent))

    val dEventSeq = (requestEvents(after = EventId.BeforeFirst, timeout = 0.s, limit = 1) await 99.s).asInstanceOf[MyNonEmptyEventSeq]
    assert((dEventSeq.stampeds map { _.value }) == List(aKeyedEvent))
  }

  private def requestEvents(after: EventId, timeout: Duration, limit: Int = Int.MaxValue): Future[EventSeq[Seq, KeyedEvent[OrderEvent]]] = {
    val promise = Promise[EventSeq[Seq, KeyedEvent[OrderEvent]]]
    eventQueue ! EventQueue.Input.RequestEvents(after = after, timeout = timeout, limit = limit, promise)
    promise.future
  }
}

private object EventQueueTest {
  private type MyNonEmptyEventSeq = EventSeq.NonEmpty[Seq, KeyedEvent[OrderEvent]]
  private type MyEmptyEventSeq = EventSeq.Empty
}
