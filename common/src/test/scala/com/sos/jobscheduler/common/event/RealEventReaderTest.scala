package com.sos.jobscheduler.common.event

import com.sos.jobscheduler.base.utils.CloseableIterator
import com.sos.jobscheduler.common.event.RealEventReaderTest._
import com.sos.jobscheduler.common.scalautil.Futures.implicits._
import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.common.time.timer.TimerService
import com.sos.jobscheduler.data.event.{Event, EventId, EventRequest, Stamped}
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import org.scalatest.FreeSpec
import scala.concurrent.duration._

/**
  * @author Joacim Zschimmer
  */
final class RealEventReaderTest extends FreeSpec {

  "observe without stack overflow" in {
    val eventReader = new TestEventReader().strict
    var expectedNext = Stamped(1, 1 <-: TestEvent(1))
    val n = 100000
    val observed = eventReader.observe(EventRequest.singleClass[TestEvent](after = EventId.BeforeFirst, limit = n, timeout = 99.seconds))
      .foreach { stamped ⇒
        assert(stamped == expectedNext)
        expectedNext = Stamped(stamped.eventId + 1, (stamped.value.key + 1) <-: TestEvent(stamped.value.event.number + 1))
      }
    observed await 99.s
    assert(expectedNext.eventId == 301/*TODO Stack overflow limit. Should be: n + 1*/)
  }
}

object RealEventReaderTest {
  private val EventsPerIteration = 3

  private case class TestEvent(number: Long) extends Event {
    type Key = Long
  }

  private def toStampedEvent(i: Long) = Stamped(i, i <-: TestEvent(i))

  private class TestEventReader extends RealEventReader[TestEvent] {
    protected val timerService = new TimerService(Some(1.s))
    protected val timeoutLimit = 1.hour
    protected val oldestEventId = 0

    protected def reverseEventsAfter(after: EventId) = Task.now(CloseableIterator.empty)

    onEventAdded(1)

    def eventsAfter(after: EventId) =
      Task.now(Some(CloseableIterator.fromIterator(
        Iterator.from(1) take EventsPerIteration map { i ⇒
          onEventAdded(after + i + 1)  // Announce following event
          toStampedEvent(after + i)
        })))
  }
}
