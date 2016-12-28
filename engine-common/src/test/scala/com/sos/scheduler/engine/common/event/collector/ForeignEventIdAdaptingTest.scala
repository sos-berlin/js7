package com.sos.scheduler.engine.common.event.collector

import com.sos.scheduler.engine.common.event.EventIdGenerator
import com.sos.scheduler.engine.common.event.collector.ForeignEventIdAdaptingTest._
import com.sos.scheduler.engine.common.scalautil.Futures.implicits.SuccessFuture
import com.sos.scheduler.engine.common.time.ScalaTime._
import com.sos.scheduler.engine.common.time.timer.TimerService
import com.sos.scheduler.engine.data.event.{EventId, EventRequest, EventSeq, KeyedEvent, NoKeyEvent, Snapshot}
import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.junit.JUnitRunner
import scala.concurrent.ExecutionContext
import scala.concurrent.ExecutionContext.Implicits.global

/**
  * @author Joacim Zschimmer
  */
@RunWith(classOf[JUnitRunner])
final class ForeignEventIdAdaptingTest extends FreeSpec {

  "test" in {
    val eventCollector =
      new EventCollector(
        EventCollector.Configuration(queueSize = 1000, timeoutLimit = 1.h),
        TimerService())
      with ForeignEventIdAdapting {
        protected val executionContext = ExecutionContext.global
        protected val eventIdGenerator = new EventIdGenerator
      }

    eventCollector.putForeignEventSnapshot(Snapshot(EventId(1), KeyedEvent(AEvent)))
    eventCollector.putForeignEventSnapshot(Snapshot(EventId.MaxValue - 100, KeyedEvent(AEvent)))

    val snapshots: Vector[Snapshot[KeyedEvent[AEvent.type]]] =
      (for (eventSeq ← eventCollector.when(EventRequest[AEvent.type](EventId.BeforeFirst, timeout = 0.s))) yield
        eventSeq match {
          case EventSeq.NonEmpty(iterator) ⇒ iterator.toVector
        }
      ) await  10.s
    assert((snapshots map { _.value }) == Vector(KeyedEvent(AEvent), KeyedEvent(AEvent)))
  }
}

object ForeignEventIdAdaptingTest {
  private object AEvent extends NoKeyEvent
}
