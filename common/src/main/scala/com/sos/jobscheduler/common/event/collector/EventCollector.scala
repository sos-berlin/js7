package com.sos.jobscheduler.common.event.collector

import com.sos.jobscheduler.base.problem.Problem
import com.sos.jobscheduler.base.utils.CloseableIterator
import com.sos.jobscheduler.common.event.RealEventWatch
import com.sos.jobscheduler.common.event.collector.EventCollector._
import com.sos.jobscheduler.data.event.{AnyKeyedEvent, EventId, Stamped}
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

  final def tornEventId: EventId =
    keyedEventQueue.tornEventId

  final def lastFileTornEventId = tornEventId

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
