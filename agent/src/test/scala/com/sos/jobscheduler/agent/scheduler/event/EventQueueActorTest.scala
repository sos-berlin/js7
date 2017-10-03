package com.sos.jobscheduler.agent.scheduler.event

import akka.actor.{ActorSystem, Props}
import akka.pattern.ask
import akka.util.Timeout
import com.softwaremill.tagging.Tagger
import com.sos.jobscheduler.agent.scheduler.event.EventQueueActorTest._
import com.sos.jobscheduler.common.scalautil.Futures.implicits._
import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.common.time.timer.TimerService
import com.sos.jobscheduler.data.event.{EventId, EventSeq, KeyedEvent, Stamped}
import com.sos.jobscheduler.data.order.OrderEvent.OrderReady
import com.sos.jobscheduler.data.order.{OrderEvent, OrderId}
import java.time.Duration
import org.scalatest.{BeforeAndAfterAll, FreeSpec}
import scala.collection.immutable.Seq
import scala.collection.mutable
import scala.concurrent.duration.DurationInt
import scala.concurrent.{Future, Promise}

/**
  * @author Joacim Zschimmer
  */
final class EventQueueActorTest extends FreeSpec with BeforeAndAfterAll {

  private val actorSystem = ActorSystem("EventQueueActorTest")
  private val timerService = new TimerService(idleTimeout = Some(1.s))
  private val actor = actorSystem.actorOf(Props { new EventQueueActor(timerService) }).taggedWith[EventQueueActor]
  private var lastEventId = EventId.BeforeFirst
  private implicit val askTimeout = Timeout(99.seconds)

  override def afterAll() = {
    actorSystem.terminate()
    super.afterAll()
  }

  "test" in {
    assert((requestEvents(after = EventId.BeforeFirst, timeout = 0.s) await 99.s) == EventSeq.Empty(EventId.BeforeFirst))
    assert((requestEvents(after = EventId.BeforeFirst, timeout = 10.ms) await 99.s) == EventSeq.Empty(EventId.BeforeFirst))

    val aKeyedEvent = KeyedEvent(OrderReady)(OrderId("1"))
    (actor ? Stamped(111, aKeyedEvent)) await 99.s
    val aEventSeq = (requestEvents(after = EventId.BeforeFirst, timeout = 0.s) await 99.s).asInstanceOf[MyNonEmptyEventSeq]
    assert((aEventSeq.stampeds map { _.value }) == List(aKeyedEvent))
    val aEventId = aEventSeq.stampeds.last.eventId
    assert(aEventId == 111)

    val whenBEventSeq = requestEvents(after = aEventId, timeout = 99.s).mapTo[EventSeq.NonEmpty[Seq, KeyedEvent[OrderEvent]]]
    sleep(50.ms)
    val bKeyedEvent = KeyedEvent(OrderReady)(OrderId("1"))
    (actor ? Stamped(222, bKeyedEvent)) await 99.s
    val bEventSeq = whenBEventSeq await 99.s
    assert((bEventSeq.stampeds map { _.value }) == List(bKeyedEvent))
    assert(bEventSeq.stampeds.last.eventId > EventId.BeforeFirst)

    val cEventSeq = (requestEvents(after = EventId.BeforeFirst, timeout = 0.s) await 99.s).asInstanceOf[MyNonEmptyEventSeq]
    assert((cEventSeq.stampeds map { _.value }) == List(aKeyedEvent, bKeyedEvent))

    val dEventSeq = (requestEvents(after = EventId.BeforeFirst, timeout = 0.s, limit = 1) await 99.s).asInstanceOf[MyNonEmptyEventSeq]
    assert((dEventSeq.stampeds map { _.value }) == List(aKeyedEvent))

    val eEventSeq = (requestEvents(after = EventId.BeforeFirst, timeout = 0.s) await 99.s).asInstanceOf[MyNonEmptyEventSeq]
    assert((eEventSeq.stampeds map { _.value }) == List(aKeyedEvent, bKeyedEvent))
    lastEventId = eEventSeq.stampeds.last.eventId
  }

  "Many events" in {
    for (_ ← 1 to 100) {
      val whenAEventSeq = requestEvents(after = lastEventId, timeout = 99.s)
      val keyedEvent = KeyedEvent(OrderReady)(OrderId("2"))
      val sent = for (i ← 1 to 1000) yield Stamped(lastEventId + i, keyedEvent)
      sent foreach actor.?  // Don't await Futures
      val received = mutable.Buffer[Stamped[KeyedEvent[OrderEvent]]]()
      received ++= (whenAEventSeq await 99.s).asInstanceOf[MyNonEmptyEventSeq].stampeds
      lastEventId = received.last.eventId
      while (lastEventId < sent.last.eventId) {
        received ++= (requestEvents(after = lastEventId, timeout = 0.s) await 99.s).asInstanceOf[MyNonEmptyEventSeq].stampeds
        lastEventId = received.last.eventId
      }
      assert(received.toVector == sent)
    }
  }

  private def requestEvents(after: EventId, timeout: Duration, limit: Int = Int.MaxValue): Future[EventSeq[Seq, KeyedEvent[OrderEvent]]] = {
    val promise = Promise[EventSeq[Seq, KeyedEvent[OrderEvent]]]
    actor ! EventQueueActor.Input.RequestEvents(after = after, timeout = timeout, limit = limit, promise)
    promise.future
  }
}

private object EventQueueActorTest {
  private type MyNonEmptyEventSeq = EventSeq.NonEmpty[Seq, KeyedEvent[OrderEvent]]
  private type MyEmptyEventSeq = EventSeq.Empty
}
