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
import scala.collection.JavaConversions._
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
  private var lastKnownEventId = EventId.BeforeFirst

  def receive = {
    case msg @ Stamped(_, KeyedEvent(_: OrderId, _: OrderEvent)) ⇒
      val stamped = msg.asInstanceOf[Stamped[KeyedEvent[OrderEvent]]]
      stamped.value match {
        case KeyedEvent(orderId: OrderId, OrderDetached) ⇒
          // Master posts its own OrderDetached
          removeEventsFor(orderId)
        case _ ⇒
          add(stamped)
      }

    case Input.RequestEvents(after, timeout, limit, promise) ⇒
      if (after != lastKnownEventId && !eventQueue.containsKey(after)) {
        val error = s"RequestEvents: unknown EventId after=${EventId.toString(after)} (lastKnownEventId=$lastKnownEventId)"
        logger.debug(error)
        sender() ! Status.Failure(new IllegalArgumentException(error))  // TODO Does requester handle Status.Failure ?
      } else {
        val stampeds = (eventQueue.navigableKeySet.tailSet(after, false).iterator take limit map eventQueue.get).toVector
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

    case Internal.RequestTimedOut(promise, emptyEventSeq) ⇒
      requestors -= promise
      promise.success(emptyEventSeq)

    case Input.GetSnapshot ⇒
      sender() ! CompleteSnapshot(lastKnownEventId, eventQueue.values.toVector)

    case CompleteSnapshot(eventId, stampedEvents) ⇒
      assert(eventQueue.isEmpty)
      lastKnownEventId = eventId
      eventQueue ++= stampedEvents map { o ⇒ java.lang.Long.valueOf(o.eventId) → o }
      logger.debug(s"${stampedEvents.size} events recovered, lastKnownEventId=${EventId.toString(lastKnownEventId)}")
  }

  private def add(stamped: Stamped[KeyedEvent[OrderEvent]]): Unit = {
    eventQueue.put(stamped.eventId, stamped)
    for ((promise, timer) ← requestors) {
      timerService.cancel(timer)
      promise.success(EventSeq.NonEmpty(List(stamped)))
    }
    requestors.clear()
  }

  private def removeEventsFor(orderId: OrderId): Unit = {
    val i = eventQueue.values.iterator
    while (i.hasNext) {
      val e = i.next()
      if (e.value.key == orderId) {
        i.remove()
        if (lastKnownEventId < e.eventId) {
          lastKnownEventId = e.eventId
        }
      }
    }
  }
}

object EventQueue {
  private val logger = Logger(getClass)

  private type StampedEvent = Stamped[KeyedEvent[OrderEvent]]
  type MyEventSeq = EventSeq[Seq, KeyedEvent[OrderEvent]]

  sealed trait Input
  object Input {
    final case class RequestEvents(after: EventId, timeout: Duration, limit: Int, result: Promise[MyEventSeq]) extends Input
    final case object GetSnapshot
  }

  final case class CompleteSnapshot(lastKnownEventId: EventId, events: Seq[StampedEvent])

  object CompleteSnapshot {
    implicit val jsonFormat = jsonFormat2(apply) withTypeName "EventQueue.Snapshot"
  }

  private object Internal {
    case class RequestTimedOut(promise: Promise[MyEventSeq], eventSeq: EventSeq.Empty)
  }
}
