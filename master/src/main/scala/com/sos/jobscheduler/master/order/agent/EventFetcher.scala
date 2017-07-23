package com.sos.jobscheduler.master.order.agent

import com.sos.jobscheduler.base.generic.Completed
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.common.time.timer.TimerService
import com.sos.jobscheduler.data.event.{Event, EventId, EventRequest, EventSeq, KeyedEvent, Stamped}
import com.sos.jobscheduler.master.order.agent.EventFetcher._
import scala.collection.immutable.Seq
import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.reflect.ClassTag
import scala.util.{Failure, Success}

/**
  * @author Joacim Zschimmer
  */
abstract class EventFetcher[E <: Event: ClassTag](after: EventId)
  (implicit timerService: TimerService, protected val executionContext: ExecutionContext)
extends AutoCloseable {

  protected val delay = 0.ms

  protected def fetchEvents(request: EventRequest[E]): Future[EventSeq[Seq, KeyedEvent[E]]]

  protected def onEvent(stamped: Stamped[KeyedEvent[E]]): Unit

  private var count = 0
  @volatile private var closed = false

  def start(): Future[Completed] =
    fetch(after)

  private def fetch(after: EventId): Future[Completed] = {
    val promise = Promise[Completed]()
    def loop(after: EventId): Unit = {
      if (closed)
        promise.success(Completed)
      else {
        timerService.delayed(delay) flatMap { _ ⇒
          fetchEvents(EventRequest.singleClass(after = after, EventTimeout))
        } onComplete {
          case Failure(t) ⇒
            promise.failure(t)
          case Success(EventSeq.NonEmpty(stampeds)) ⇒
            for (stamped ← stampeds if !closed) {
              count += 1
              logger.trace(s"#$count $stamped")
              onEvent(stamped)
            }
            loop(stampeds.last.eventId)
          case Success(EventSeq.Empty(lastEventId)) ⇒
            loop(lastEventId)
        }
      }
    }
    loop(after)
    promise.future
  }

  def close(): Unit = {
    closed = true
  }
}

object EventFetcher {
  private val logger = Logger(getClass)
  private val EventTimeout = 60.s
}
