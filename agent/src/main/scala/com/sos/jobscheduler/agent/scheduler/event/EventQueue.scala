package com.sos.jobscheduler.agent.scheduler.event

import akka.actor.{Actor, Status}
import com.sos.jobscheduler.agent.scheduler.event.EventQueue._
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.common.time.timer.{Timer, TimerService}
import com.sos.jobscheduler.data.engine2.order.OrderEvent
import com.sos.jobscheduler.data.engine2.order.OrderEvent.OrderDetached
import com.sos.jobscheduler.data.event.{EventId, EventSeq, KeyedEvent, Snapshot}
import com.sos.jobscheduler.data.order.OrderId
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
  private val eventQueue = new java.util.concurrent.ConcurrentSkipListMap[java.lang.Long, EventSnapshot]
  private val requestors = mutable.Map[Promise[MyEventSeq], Timer[Unit]]()
  private var lastKnownEventId = EventId.BeforeFirst

  def receive = {
    case msg @ Snapshot(_, KeyedEvent(_: OrderId, _: OrderEvent)) ⇒
      val eventSnapshot = msg.asInstanceOf[Snapshot[KeyedEvent[OrderEvent]]]
      eventSnapshot.value match {
        case KeyedEvent(orderId: OrderId, OrderDetached) ⇒
          // Master posts its own OrderDetached
          removeEventsFor(orderId)
        case _ ⇒
          add(eventSnapshot)
      }

    case Input.RequestEvents(after, timeout, limit, promise) ⇒
      if (after != lastKnownEventId && !eventQueue.containsKey(after)) {
        val error = s"RequestEvents: unknown EventId after=${EventId.toString(after)} (lastKnownEventId=$lastKnownEventId)"
        logger.debug(error)
        sender() ! Status.Failure(new IllegalArgumentException(error))  // TODO Does requester handle Status.Failure ?
      } else {
        val eventSnapshots = (eventQueue.navigableKeySet.tailSet(after, false).iterator take limit map eventQueue.get).toVector
        if (eventSnapshots.nonEmpty) {
          promise.success(EventSeq.NonEmpty(eventSnapshots))
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

    case CompleteSnapshot(eventId, eventSnapshots) ⇒
      assert(eventQueue.isEmpty)
      lastKnownEventId = eventId
      eventQueue ++= eventSnapshots map { o ⇒ java.lang.Long.valueOf(o.eventId) → o }
      logger.debug(s"${eventSnapshots.size} events recovered, lastKnownEventId=${EventId.toString(lastKnownEventId)}")
  }

  private def add(eventSnapshot: Snapshot[KeyedEvent[OrderEvent]]): Unit = {
    eventQueue.put(eventSnapshot.eventId, eventSnapshot)
    for ((promise, timer) ← requestors) {
      timerService.cancel(timer)
      promise.success(EventSeq.NonEmpty(List(eventSnapshot)))
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

  private type EventSnapshot = Snapshot[KeyedEvent[OrderEvent]]
  type MyEventSeq = EventSeq[Seq, KeyedEvent[OrderEvent]]

  sealed trait Input
  object Input {
    final case class RequestEvents(after: EventId, timeout: Duration, limit: Int, result: Promise[MyEventSeq]) extends Input
    final case object GetSnapshot
  }

  final case class CompleteSnapshot(lastKnownEventId: EventId, eventSnapshots: Seq[EventSnapshot])

  object CompleteSnapshot {
    implicit val jsonFormat = jsonFormat2(apply)  // TODO Eindeutiger TYPE
  }

  private object Internal {
    case class RequestTimedOut(promise: Promise[MyEventSeq], eventSeq: EventSeq.Empty)
  }
}
