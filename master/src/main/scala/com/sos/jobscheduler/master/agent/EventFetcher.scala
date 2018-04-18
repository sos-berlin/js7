package com.sos.jobscheduler.master.agent

import com.sos.jobscheduler.base.generic.Completed
import com.sos.jobscheduler.base.utils.StackTraces.StackTraceThrowable
import com.sos.jobscheduler.common.configutils.Configs.ConvertibleConfig
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.common.time.timer.TimerService
import com.sos.jobscheduler.data.event.{Event, EventId, EventRequest, EventSeq, KeyedEvent, Stamped}
import com.sos.jobscheduler.master.agent.EventFetcher._
import com.typesafe.config.Config
import java.time.Duration
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

  protected def config: Config

  protected def fetchEvents(request: EventRequest[E]): Future[EventSeq[Seq, KeyedEvent[E]]]

  protected def onEvents(stamped: Seq[Stamped[KeyedEvent[E]]]): Unit

  protected lazy val delay = config.as[Duration]("jobscheduler.master.agent-driver.event-fetcher.delay")
  private var logCount = 0
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
            promise.failure(t.appendCurrentStackTrace)
          case Success(EventSeq.NonEmpty(stampeds)) ⇒
            if (logger.underlying.isTraceEnabled) for (stamped ← stampeds) { logCount += 1; logger.trace(s"#$logCount $stamped") }
            onEvents(stampeds)
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
  private val EventTimeout = 50.s
}
