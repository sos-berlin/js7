package com.sos.jobscheduler.common.event.collector

import com.sos.jobscheduler.common.event.EventReader
import com.sos.jobscheduler.common.event.collector.EventCollector._
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.common.time.timer.TimerService
import com.sos.jobscheduler.data.event.{AnyKeyedEvent, Event, EventId, Stamped}
import java.time.Duration
import scala.concurrent.{ExecutionContext, Future}

/**
  * @author Joacim Zschimmer
  */
abstract class EventCollector(configuration: Configuration)
  (implicit
    protected val timerService: TimerService,
    protected val executionContext: ExecutionContext)
extends EventReader[Event]
{
  protected def timeoutLimit = configuration.timeoutLimit
  private[collector] val keyedEventQueue = new MemoryKeyedEventQueue(sizeLimit = configuration.queueSize)

  logger.debug("oldestEventId=" + EventId.toString(oldestEventId))

  final def addStamped(stamped: Stamped[AnyKeyedEvent]): Unit = {
    keyedEventQueue.add(stamped)
    onEventAdded(stamped.eventId)
  }

  final def oldestEventId: EventId =
    keyedEventQueue.oldestEventId

  protected final def eventsAfter(after: EventId) =
    Future.successful(keyedEventQueue.after(after))

  protected final def reverseEventsAfter(after: EventId) =
    Future.successful(keyedEventQueue.reverseEvents(after = after))
}

object EventCollector {
  private val logger = Logger(getClass)

  final case class Configuration(
    queueSize: Int,
    /** Limits open requests, and avoids arithmetic overflow. */
    timeoutLimit: Duration)

  object Configuration {
    val ForTest = Configuration(queueSize = 1000, timeoutLimit = 600.s)
  }

  final class ForTest(
    configuration: Configuration = Configuration.ForTest)
    (implicit
      timerService: TimerService,
      executionContext: ExecutionContext)
    extends EventCollector(configuration)
}
