package com.sos.jobscheduler.agent.scheduler.event

import akka.actor.Props
import akka.pattern.ask
import akka.util.Timeout
import com.softwaremill.tagging.Tagger
import com.sos.jobscheduler.agent.scheduler.event.EventQueueActorTest._
import com.sos.jobscheduler.base.time.Timestamp
import com.sos.jobscheduler.common.akkautils.Akkas.newActorSystem
import com.sos.jobscheduler.common.scalautil.Futures.implicits._
import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.common.time.Stopwatch
import com.sos.jobscheduler.common.time.timer.TimerService
import com.sos.jobscheduler.data.event.{EventId, EventSeq, KeyedEvent, Stamped}
import com.sos.jobscheduler.data.order.OrderEvent.{OrderDetachable, OrderDetached, OrderFinished}
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

  private val actorSystem = newActorSystem("EventQueueActorTest")
  private val timerService = new TimerService(idleTimeout = Some(1.s))
  private val actor = actorSystem.actorOf(Props { new EventQueueActor(timerService) }, "EventQueueActor").taggedWith[EventQueueActor]
  private var lastEventId = EventId.BeforeFirst
  private implicit val askTimeout = Timeout(99.seconds)

  override def afterAll() = {
    actorSystem.terminate()
    super.afterAll()
  }

  "test" in {
    assert((requestEvents(after = EventId.BeforeFirst, timeout = 0.s) await 99.s) == EventSeq.Empty(EventId.BeforeFirst))
    assert((requestEvents(after = EventId.BeforeFirst, timeout = 10.ms) await 99.s) == EventSeq.Empty(EventId.BeforeFirst))

    val aKeyedEvent = KeyedEvent(OrderDetachable)(OrderId("1"))
    actor ! Stamped(111, Timestamp.ofEpochMilli(0), aKeyedEvent)
    val aEventSeq = (requestEvents(after = EventId.BeforeFirst, timeout = 0.s) await 99.s).asInstanceOf[MyNonEmptyEventSeq]
    assert((aEventSeq.stampeds map { _.value }) == List(aKeyedEvent))
    val aEventId = aEventSeq.stampeds.last.eventId
    assert(aEventId == 111)

    val whenBEventSeq = requestEvents(after = aEventId, timeout = 99.s).mapTo[EventSeq.NonEmpty[Seq, KeyedEvent[OrderEvent]]]
    sleep(50.ms)
    val bKeyedEvent = KeyedEvent(OrderDetachable)(OrderId("1"))
    actor ! Stamped(222, Timestamp.ofEpochMilli(0), bKeyedEvent)
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
      val keyedEvent = KeyedEvent(OrderDetachable)(OrderId("2"))
      val sent = for (i ← 1 to 1000) yield Stamped(lastEventId + i, Timestamp.ofEpochMilli(i), keyedEvent)
      sent foreach actor.!
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

  for (n ← sys.props.get("test.speed") map (_.toInt)) s"Speed test ×$n" in {
    val testEvent = KeyedEvent(OrderFinished)(OrderId("1"))
    val events = for (i ← 0 until n) yield Stamped(1000000 + i, Timestamp.ofEpochMilli(i), testEvent)
    val stopwatch = new Stopwatch
    for (e ← events) actor ! e
    info(stopwatch.itemsPerSecondString(n, "events"))
  }

  if (sys.props contains "test.speed") s"Speed test OrderDetached" in {
    val n = 1000
    val orderDetached = KeyedEvent(OrderDetached)(OrderId("1"))
    val events = for (i ← 0 until n) yield Stamped(1000000 + i, Timestamp.ofEpochMilli(i), orderDetached)
    val stopwatch = new Stopwatch
    for (e ← events) (actor ? e) await 99.s
    info(stopwatch.itemsPerSecondString(n, "OrderDetached"))
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
