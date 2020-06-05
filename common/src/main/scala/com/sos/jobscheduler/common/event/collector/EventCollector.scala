package js7.common.event.collector

import js7.base.problem.Problem
import js7.base.utils.CloseableIterator
import js7.common.event.RealEventWatch
import js7.common.event.collector.EventCollector._
import js7.data.event.{AnyKeyedEvent, EventId, Stamped}
import monix.execution.Scheduler
import scala.concurrent.duration.FiniteDuration

/**
  * @author Joacim Zschimmer
  */
abstract class EventCollector(configuration: Configuration)(implicit protected val scheduler: Scheduler)
extends RealEventWatch
{
  private[collector] val keyedEventQueue = new MemoryKeyedEventQueue(sizeLimit = configuration.queueSize)

  final def addStamped(stamped: Stamped[AnyKeyedEvent]): Unit = {
    keyedEventQueue.add(stamped)
    onEventsCommitted(stamped.eventId)
  }

  def tear(after: EventId): Unit =
    keyedEventQueue.tear(after)

  def fileEventIds = keyedEventQueue.tornEventId :: Nil

  def eventsAfter(after: EventId) =
    keyedEventQueue.after(after) map CloseableIterator.fromIterator

  final def observeFile(fileEventId: Option[EventId], position: Option[Long], timeout: FiniteDuration, markEOF: Boolean, onlyLastOfChunk: Boolean) =
    Left(Problem("EventCollector.observeFile is not implemented"))
}

object EventCollector
{
  final case class Configuration(queueSize: Int)

  object Configuration {
    val ForTest = Configuration(queueSize = 1000)
  }

  final class ForTest(
    configuration: Configuration = Configuration.ForTest)
    (implicit scheduler: Scheduler)
    extends EventCollector(configuration)
  {
    def snapshotObjectsFor(after: EventId) = None
  }
}
