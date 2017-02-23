package com.sos.scheduler.engine.master

import akka.actor.{Actor, ActorRefFactory, Props}
import com.sos.scheduler.engine.common.event.collector.EventCollector
import com.sos.scheduler.engine.common.scalautil.Logger
import com.sos.scheduler.engine.common.time.ScalaTime._
import com.sos.scheduler.engine.common.time.timer.TimerService
import com.sos.scheduler.engine.data.event.{AnyKeyedEvent, Event, EventId, Snapshot}
import com.sos.scheduler.engine.master.TestEventCollector._
import com.sos.scheduler.engine.shared.event.SnapshotKeyedEventBus
import scala.concurrent.ExecutionContext

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
  def start(actorRefFactory: ActorRefFactory, keyedEventBus: SnapshotKeyedEventBus): Unit = {
    actorRefFactory.actorOf(
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
            case event: Snapshot[AnyKeyedEvent] ⇒
              logger.debug(event.toString)
              putEventSnapshot(event)
          }
        }
      },
      name = "TestEventCollector")
  }
}

object TestEventCollector {
  private val logger = Logger(getClass)
}
