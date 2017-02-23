package com.sos.jobscheduler.master.order.agent

import com.sos.jobscheduler.common.scalautil.Futures.implicits._
import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.common.time.timer.TimerService
import com.sos.jobscheduler.data.event.{EventRequest, EventSeq, KeyedEvent, NoKeyEvent, Snapshot}
import com.sos.jobscheduler.master.order.agent.EventFetcherTest._
import java.util.concurrent.CountDownLatch
import org.scalatest.FreeSpec
import scala.collection.mutable
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

/**
  * @author Joacim Zschimmer
  */
final class EventFetcherTest extends FreeSpec {

  "test" in {
    implicit val timerService = TimerService(idleTimeout = Some(1.s))
    val collector = mutable.Buffer[Snapshot[KeyedEvent[TestEvent]]]()
    val aBarrier, bBarrier, cBarrier, dBarrier = new CountDownLatch(1)
    val fetcher = new EventFetcher[TestEvent](after = 100) {
      var step = 0
      def fetchEvents(request: EventRequest[TestEvent]) =
        Future {
          step += 1
          step match {
            case 1 ⇒
              assert(request.after == 100)
              EventSeq.NonEmpty(List(
                Snapshot(101, KeyedEvent(AEvent(1))),
                Snapshot(102, KeyedEvent(AEvent(2)))))
            case 2 ⇒
              aBarrier.countDown()
              bBarrier.await()
              assert(request.after == 102)
              EventSeq.Empty(lastEventId = 200)
            case 3 ⇒
              assert(request.after == 200)
              EventSeq.NonEmpty(List(
                Snapshot(201, KeyedEvent(AEvent(3)))))
            case 4 ⇒
              cBarrier.countDown()
              dBarrier.await()
              assert(request.after == 201)
              EventSeq.Empty(lastEventId = 201)
          }
        }

      def onEvent(eventSnapshot: Snapshot[KeyedEvent[TestEvent]]) =
        collector += eventSnapshot
    }

    val whenCompleted = fetcher.start()
    aBarrier.await()
    assert(collector == List(
      Snapshot(101, KeyedEvent(AEvent(1))),
      Snapshot(102, KeyedEvent(AEvent(2)))))
    bBarrier.countDown()
    cBarrier.await()
    assert(!whenCompleted.isCompleted)
    dBarrier.countDown()
    fetcher.close()
    whenCompleted await 3.s
    assert(collector == List(
      Snapshot(101, KeyedEvent(AEvent(1))),
      Snapshot(102, KeyedEvent(AEvent(2))),
      Snapshot(201, KeyedEvent(AEvent(3)))))
  }
}

object EventFetcherTest {
  private trait TestEvent extends NoKeyEvent
  private case class AEvent(number: Long) extends TestEvent
}
