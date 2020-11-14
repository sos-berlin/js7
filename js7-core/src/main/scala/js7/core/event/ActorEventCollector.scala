package js7.core.event

import akka.actor.{Actor, ActorSystem, Props}
import javax.inject.{Inject, Singleton}
import js7.common.event.collector.EventCollector
import js7.common.scalautil.Logger
import js7.core.event.ActorEventCollector._
import js7.data.event.{AnyKeyedEvent, Event, EventId, KeyedEvent, Stamped}
import monix.execution.Scheduler

/**
  * @author Joacim Zschimmer
  */
final class ActorEventCollector private(
  isCollectable: Event => Boolean)(
  configuration: EventCollector.Configuration,
  scheduler: Scheduler,
  keyedEventBus: StampedKeyedEventBus,
  actorSystem: ActorSystem)
extends EventCollector(configuration)(scheduler)
with AutoCloseable
{
  def isActiveNode = true

  def snapshotAfter(after: EventId) = None

  def rawSnapshotAfter(after: EventId) = None

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
          case stamped @ Stamped(_, _, KeyedEvent(_, event)) =>
            if (isCollectable(event)) {
              addStamped(stamped.asInstanceOf[Stamped[AnyKeyedEvent]])
            }
        }
      }
    },
    name = "EventCollector")

  def close() = actorSystem.stop(actorRef)

  def journalInfo = throw new NotImplementedError
}

object ActorEventCollector {
  private val logger = Logger(getClass)

  @Singleton
  final class Factory @Inject private(
    configuration: EventCollector.Configuration,
    scheduler: Scheduler,
    keyedEventBus: StampedKeyedEventBus,
    actorSystem: ActorSystem)
  {
    def apply() = filterEvents(_ => true)

    def filterEvents(isEventCollectable: Event => Boolean) =
      new ActorEventCollector(
        isEventCollectable)(
        configuration,
        scheduler,
        keyedEventBus,
        actorSystem)
  }
}
