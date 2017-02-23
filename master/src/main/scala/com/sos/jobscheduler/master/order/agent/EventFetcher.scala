package com.sos.jobscheduler.master.order.agent

import com.sos.jobscheduler.base.generic.Completed
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.common.time.timer.TimerService
import com.sos.jobscheduler.data.event.{Event, EventId, EventRequest, EventSeq, KeyedEvent, Snapshot}
import com.sos.jobscheduler.master.order.agent.EventFetcher._
import scala.collection.immutable.Seq
import scala.concurrent.{ExecutionContext, Future}
import scala.reflect.ClassTag

/**
  * @author Joacim Zschimmer
  */
abstract class EventFetcher[E <: Event: ClassTag](after: EventId)
  (implicit timerService: TimerService, protected val executionContext: ExecutionContext)
extends AutoCloseable {

  protected val delay = 10.ms

  def fetchEvents(request: EventRequest[E]): Future[EventSeq[Seq, KeyedEvent[E]]]

  def onEvent(eventSnapshot: Snapshot[KeyedEvent[E]]): Unit

  private var count = 0
  @volatile private var closed = false

  def start(): Future[Completed] =
    fetch(after)

  private def fetch(after: EventId): Future[Completed] =
    if (closed)
      Future.successful(Completed)
    else
      for (_ ← timerService.delayed(delay);
           eventSeq ← fetchEvents(EventRequest.singleClass(after = after, EventTimeout));
           completed ← eventSeq match {
              case EventSeq.NonEmpty(eventSnapshots) ⇒
                for (eventSnapshot ← eventSnapshots if !closed) {
                  count += 1
                  logger.trace(s"#$count $eventSnapshot")
                  onEvent(eventSnapshot)
                }
                fetch(eventSnapshots.last.eventId)
              case EventSeq.Empty(lastEventId) ⇒
                fetch(lastEventId)
              case EventSeq.Torn ⇒  // Agent's event stream never tears (maybe we should replace EventSeq something simpler)
                Future.failed(new RuntimeException(s"EventSeq is torn (after=${EventId.toString(after)}, $count events)"))
            })
        yield completed

  def close(): Unit = {
    closed = true
  }
}

object EventFetcher {
  private val logger = Logger(getClass)
  private val EventTimeout = 60.s
}
