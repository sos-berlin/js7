package com.sos.jobscheduler.master.tests

import akka.actor.{Actor, ActorRefFactory, Props}
import com.sos.jobscheduler.common.event.collector.EventCollector
import com.sos.jobscheduler.common.scalautil.Futures.implicits._
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.common.time.timer.TimerService
import com.sos.jobscheduler.core.event.StampedKeyedEventBus
import com.sos.jobscheduler.data.event.{AnyKeyedEvent, Event, EventId, EventRequest, EventSeq, KeyedEvent, Stamped, TearableEventSeq}
import com.sos.jobscheduler.master.tests.TestEventCollector._
import scala.concurrent.ExecutionContext
import scala.reflect.ClassTag

/**
  * @author Joacim Zschimmer
  */
final class TestEventCollector
extends EventCollector(
  initialOldestEventId = EventId.BeforeFirst,
  EventCollector.Configuration.ForTest)(
  TimerService(idleTimeout = Some(1.s)),
  ExecutionContext.global)
{
  def start(actorRefFactory: ActorRefFactory, keyedEventBus: StampedKeyedEventBus): Unit = {
    val actor = actorRefFactory.actorOf(
      Props {
        new Actor {
          logger.trace("Ready")

          override def postStop() = {
            keyedEventBus.unsubscribe(self)
            logger.trace("Stopped")
            super.postStop()
          }

          def receive = {
            case stamped: Stamped[AnyKeyedEvent] @unchecked ⇒
              logger.trace(stamped.toString)
              addStamped(stamped)
          }
        }
      },
      name = "TestEventCollector")
    keyedEventBus.subscribe(actor, classOf[Event])  // Subscribe before return
  }

  def await[E <: Event: ClassTag](
    predicate: KeyedEvent[E] ⇒ Boolean = (_: KeyedEvent[E]) ⇒ true,
    after: EventId = EventId.BeforeFirst)(
    implicit ec: ExecutionContext)
  : Vector[Stamped[KeyedEvent[E]]] =
    when[E](EventRequest.singleClass[E](after = after, 99.s), predicate) await 99.s match {
      case EventSeq.NonEmpty(events) ⇒ events.toVector
      case o ⇒ sys.error(s"Unexpected EventSeq: $o")
    }

  def all[E <: Event: ClassTag]: TearableEventSeq[Iterator, KeyedEvent[E]] =
    byPredicate[E](EventRequest.singleClass(after = EventId.BeforeFirst, timeout = 0.s), _ ⇒ true) await 99.s
}

object TestEventCollector {
  private val logger = Logger(getClass)
}
