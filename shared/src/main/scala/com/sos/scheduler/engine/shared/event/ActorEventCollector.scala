package com.sos.scheduler.engine.shared.event

import akka.actor.{Actor, ActorSystem, Props}
import com.sos.scheduler.engine.common.event.EventIdGenerator
import com.sos.scheduler.engine.common.event.collector.EventCollector
import com.sos.scheduler.engine.common.scalautil.Logger
import com.sos.scheduler.engine.common.time.timer.TimerService
import com.sos.scheduler.engine.data.event.{AnyKeyedEvent, Event, Snapshot}
import javax.inject.{Inject, Singleton}
import scala.concurrent.ExecutionContext
import ActorEventCollector._

/**
  * @author Joacim Zschimmer
  */
@Singleton
final class ActorEventCollector @Inject private(
  configuration: EventCollector.Configuration,
  timerService: TimerService,
  executionContext: ExecutionContext,
  protected val eventIdGenerator: EventIdGenerator,
  keyedEventBus: SnapshotKeyedEventBus,
  actorSystem: ActorSystem)
extends EventCollector(initialOldestEventId = eventIdGenerator.next(), configuration)(timerService, executionContext)
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
          case event: Snapshot[AnyKeyedEvent] ⇒ putEventSnapshot(event)
        }
      }
    },
    name = "EventCollector")

  def close() = actorSystem.stop(actorRef)
}

object ActorEventCollector {
  private val logger = Logger(getClass)
}
