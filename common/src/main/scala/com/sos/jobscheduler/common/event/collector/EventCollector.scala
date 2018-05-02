package com.sos.jobscheduler.common.event.collector

import com.sos.jobscheduler.base.generic.Completed
import com.sos.jobscheduler.common.event.EventReader
import com.sos.jobscheduler.common.event.collector.EventCollector._
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
  protected val started = Future.successful(Completed)
  private[collector] val keyedEventQueue = new MemoryKeyedEventQueue(sizeLimit = configuration.queueSize)

  final def addStamped(stamped: Stamped[AnyKeyedEvent]): Unit = {
    keyedEventQueue.add(stamped)
    onEventAdded(stamped.eventId)
  }

  /** This implementation is always safe. */
  protected final def unsafeOldestEventId: EventId =
    keyedEventQueue.oldestEventId

  protected final def eventsAfter(after: EventId) =
    Future.successful(keyedEventQueue.after(after))

  protected final def reverseEventsAfter(after: EventId) =
    Future.successful(keyedEventQueue.reverseEvents(after = after))
}

object EventCollector
{
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
