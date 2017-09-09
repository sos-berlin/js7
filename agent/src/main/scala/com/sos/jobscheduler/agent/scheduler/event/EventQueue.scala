package com.sos.jobscheduler.agent.scheduler.event

import akka.actor.{Actor, Status}
import com.sos.jobscheduler.agent.scheduler.event.EventQueue._
import com.sos.jobscheduler.base.sprayjson.typed.NamedJsonFormat.ToTypeJsonFormat
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.common.time.timer.{Timer, TimerService}
import com.sos.jobscheduler.data.event.{EventId, EventSeq, KeyedEvent, Stamped}
import com.sos.jobscheduler.data.order.OrderEvent.OrderDetached
import com.sos.jobscheduler.data.order.{OrderEvent, OrderId}
import java.time.Duration
import scala.collection.JavaConverters._
import scala.collection.immutable.Seq
import scala.collection.mutable
import scala.concurrent.Promise
import spray.json.DefaultJsonProtocol._

/**
  * @author Joacim Zschimmer
  */
final class EventQueue(timerService: TimerService) extends Actor {

  private implicit val executionContext = context.dispatcher
  private val eventQueue = new java.util.concurrent.ConcurrentSkipListMap[java.lang.Long, StampedEvent]
  private val requestors = mutable.Map[Promise[MyEventSeq], Timer[Unit]]()
  private var oldestKnownEventId = EventId.BeforeFirst
  private var firstResponse = NoFirstResponse

  def receive = {
    case msg @ Stamped(_, KeyedEvent(_: OrderId, _: OrderEvent)) ⇒
      val stamped = msg.asInstanceOf[Stamped[KeyedEvent[OrderEvent]]]
      stamped.value match {
        case KeyedEvent(orderId: OrderId, OrderDetached) ⇒
          // Master posts its own OrderDetached so we can forget the order's events here
          removeEventsFor(orderId)
        case _ ⇒
          eventQueue.put(stamped.eventId, stamped)
          if (requestors.nonEmpty) {
            if (firstResponse == NoFirstResponse) {
              firstResponse = stamped.eventId
            }
            self ! Internal.OnAdded(stamped.eventId)
          }
      }

    case Input.RequestEvents(after, timeout, limit, promise) ⇒
      if (after != oldestKnownEventId && !eventQueue.containsKey(after)) {
        val error = s"RequestEvents: unknown EventId after=${EventId.toString(after)} (oldestKnownEventId=$oldestKnownEventId)"
        logger.debug(error)
        sender() ! Status.Failure(new IllegalArgumentException(error))  // TODO Does requester handle Status.Failure ?
      } else {
        val stampeds = (eventQueue.navigableKeySet.tailSet(after, false).iterator.asScala take limit map eventQueue.get).toVector
        if (stampeds.nonEmpty) {
          promise.success(EventSeq.NonEmpty(stampeds))
        } else
        if (timeout <= 0.s) {
          promise.success(EventSeq.Empty(lastEventId = after))
        } else {
          val timer = timerService.delay(timeout, "RequestEvents")
          timer onElapsed {
            self ! Internal.RequestTimedOut(promise, EventSeq.Empty(lastEventId = after))
          }
          requestors += promise → timer
        }
      }

    case Internal.OnAdded(eventId) ⇒
      if (eventId == eventQueue.lastKey/*may return null*/ && firstResponse != NoFirstResponse/*to be sure*/) {
        for ((promise, timer) ← requestors) {
          timerService.cancel(timer)
          promise.success(EventSeq.NonEmpty((eventQueue.navigableKeySet.tailSet(firstResponse, true).iterator.asScala map eventQueue.get).toVector))
        }
        firstResponse = NoFirstResponse
        requestors.clear()
      }

    case Internal.RequestTimedOut(promise, emptyEventSeq) ⇒
      requestors -= promise
      promise.trySuccess(emptyEventSeq)

    case Input.GetSnapshot ⇒
      sender() ! CompleteSnapshot(oldestKnownEventId, eventQueue.values.asScala.toVector)

    case CompleteSnapshot(eventId, stampedEvents) ⇒
      assert(eventQueue.isEmpty)
      oldestKnownEventId = eventId
      for (o ← stampedEvents)
        eventQueue.put(o.eventId, o)
      logger.debug(s"${stampedEvents.size} events recovered, oldestKnownEventId=${EventId.toString(oldestKnownEventId)}")
  }

  private def removeEventsFor(orderId: OrderId): Unit = {
    val i = eventQueue.values.iterator
    while (i.hasNext) {
      val e = i.next()
      if (e.value.key == orderId) {
        i.remove()
        if (oldestKnownEventId < e.eventId) {
          oldestKnownEventId = e.eventId
        }
      }
    }
  }
}

object EventQueue {
  private val logger = Logger(getClass)
  private val NoFirstResponse = EventId.BeforeFirst

  private type StampedEvent = Stamped[KeyedEvent[OrderEvent]]
  type MyEventSeq = EventSeq[Seq, KeyedEvent[OrderEvent]]

  sealed trait Input
  object Input {
    final case class RequestEvents(after: EventId, timeout: Duration, limit: Int, result: Promise[MyEventSeq]) extends Input
    final case object GetSnapshot
  }

  final case class CompleteSnapshot(oldestKnownEventId: EventId, events: Seq[StampedEvent])   // TODO May become big when serialized to journal

  object CompleteSnapshot {
    implicit val jsonFormat = jsonFormat2(apply) withTypeName "EventQueue.Snapshot"
  }

  private object Internal {
    case class RequestTimedOut(promise: Promise[MyEventSeq], eventSeq: EventSeq.Empty)
    case class OnAdded(eventId: EventId)
  }
}
