package com.sos.jobscheduler.core.event

import akka.actor.{Actor, ActorSystem, Props}
import com.sos.jobscheduler.common.event.collector.EventCollector
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.common.time.timer.TimerService
import com.sos.jobscheduler.core.event.ActorEventCollector._
import com.sos.jobscheduler.data.event.{AnyKeyedEvent, Event, KeyedEvent, Stamped}
import javax.inject.{Inject, Singleton}

/**
  * @author Joacim Zschimmer
  */
final class ActorEventCollector private(
  isCollectable: Event ⇒ Boolean)(
  configuration: EventCollector.Configuration,
  timerService: TimerService,
  keyedEventBus: StampedKeyedEventBus,
  actorSystem: ActorSystem)
extends EventCollector(configuration)(timerService)
with AutoCloseable {

  private val actorRef = actorSystem.actorOf(
    Props {
      new Actor {
        override def preStart() = {
          super.preStart()
          keyedEventBus.subscribe(self, classOf[Event])
          logger.debug("Ready")
        }

        override def postStop() = {
          keyedEventBus.unsubscribe(self)
          logger.debug("Stopped")
          super.postStop()
        }

        def receive = {
          case stamped @ Stamped(_, _, KeyedEvent(_, event)) ⇒
            if (isCollectable(event)) {
              addStamped(stamped.asInstanceOf[Stamped[AnyKeyedEvent]])
            }
        }
      }
    },
    name = "EventCollector")

  def close() = actorSystem.stop(actorRef)
}

object ActorEventCollector {
  private val logger = Logger(getClass)

  @Singleton
  final class Factory @Inject private(
    configuration: EventCollector.Configuration,
    timerService: TimerService,
    keyedEventBus: StampedKeyedEventBus,
    actorSystem: ActorSystem)
  {
    def apply() = filterEvents(_ ⇒ true)

    def filterEvents(isEventCollectable: Event ⇒ Boolean) =
      new ActorEventCollector(
        isEventCollectable)(
        configuration,
        timerService,
        keyedEventBus,
        actorSystem)
  }
}
