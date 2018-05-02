package com.sos.jobscheduler.master.tests

import akka.actor.{Actor, ActorRefFactory, Props}
import com.sos.jobscheduler.common.event.collector.EventCollector
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.common.time.timer.TimerService
import com.sos.jobscheduler.core.event.StampedKeyedEventBus
import com.sos.jobscheduler.data.event.{AnyKeyedEvent, Event, Stamped}
import com.sos.jobscheduler.master.tests.TestEventCollector._
import scala.concurrent.ExecutionContext

/**
  * @author Joacim Zschimmer
  */
final class TestEventCollector
extends EventCollector(
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
}

object TestEventCollector {
  private val logger = Logger(getClass)
}
