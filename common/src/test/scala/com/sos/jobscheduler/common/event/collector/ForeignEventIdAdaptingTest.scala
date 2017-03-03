package com.sos.jobscheduler.common.event.collector

import com.sos.jobscheduler.common.event.EventIdGenerator
import com.sos.jobscheduler.common.event.collector.ForeignEventIdAdaptingTest._
import com.sos.jobscheduler.common.scalautil.Futures.implicits.SuccessFuture
import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.common.time.timer.TimerService
import com.sos.jobscheduler.data.event.{EventId, EventRequest, EventSeq, KeyedEvent, NoKeyEvent, Stamped}
import org.scalatest.FreeSpec
import scala.concurrent.ExecutionContext
import scala.concurrent.ExecutionContext.Implicits.global

/**
  * @author Joacim Zschimmer
  */
final class ForeignEventIdAdaptingTest extends FreeSpec {

  "test" in {
    implicit val timerService = TimerService(idleTimeout = Some(1.s))
    val eventCollector =
      new EventCollector(initialOldestEventId = EventId.BeforeFirst, EventCollector.Configuration(queueSize = 1000, timeoutLimit = 1.h))
      with ForeignEventIdAdapting {
        protected val executionContext = ExecutionContext.global
        protected val eventIdGenerator = new EventIdGenerator
      }

    eventCollector.putForeignEventStamped(Stamped(EventId(1), KeyedEvent(AEvent)))
    eventCollector.putForeignEventStamped(Stamped(EventId.MaxValue - 100, KeyedEvent(AEvent)))

    val stampeds: Vector[Stamped[KeyedEvent[AEvent.type]]] =
      (for (eventSeq ← eventCollector.when(EventRequest.singleClass[AEvent.type](EventId.BeforeFirst, timeout = 0.s))) yield
        eventSeq match {
          case EventSeq.NonEmpty(iterator) ⇒ iterator.toVector
        }
      ) await  10.s
    assert((stampeds map { _.value }) == Vector(KeyedEvent(AEvent), KeyedEvent(AEvent)))
  }
}

object ForeignEventIdAdaptingTest {
  private object AEvent extends NoKeyEvent
}
