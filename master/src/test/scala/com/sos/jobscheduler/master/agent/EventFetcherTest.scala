package com.sos.jobscheduler.master.agent

import com.sos.jobscheduler.base.time.Timestamp
import com.sos.jobscheduler.common.scalautil.Futures.implicits._
import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.data.event.{EventRequest, EventSeq, KeyedEvent, NoKeyEvent, Stamped}
import com.sos.jobscheduler.master.agent.EventFetcherTest._
import java.util.concurrent.CountDownLatch
import java.util.concurrent.TimeUnit.SECONDS
import monix.execution.Scheduler
import org.scalatest.FreeSpec
import scala.collection.immutable.Seq
import scala.collection.mutable
import scala.concurrent.Future
import scala.concurrent.duration._
import org.scalatest.Matchers._

/**
  * @author Joacim Zschimmer
  */
final class EventFetcherTest extends FreeSpec {

  "test" in {
    val collector = mutable.Buffer[Stamped[KeyedEvent[TestEvent]]]()
    val aBarrier, bBarrier, cBarrier, dBarrier = new CountDownLatch(1)
    implicit val scheduler = Scheduler.global
    val fetcher = new EventFetcher[TestEvent](after = 100) {
      protected val delay = Duration.Zero
      var step = 0
      def fetchEvents(request: EventRequest[TestEvent]) =
        Future {
          step += 1
          step match {
            case 1 ⇒
              assert(request.after == 100)
              EventSeq.NonEmpty(List(
                Stamped(101, Timestamp.ofEpochMilli(101), KeyedEvent(AEvent(1))),
                Stamped(102, Timestamp.ofEpochMilli(102), KeyedEvent(AEvent(2)))))
            case 2 ⇒
              aBarrier.countDown()
              bBarrier.await(99, SECONDS) shouldBe true
              assert(request.after == 102)
              EventSeq.Empty(lastEventId = 200)
            case 3 ⇒
              assert(request.after == 200)
              EventSeq.NonEmpty(List(
                Stamped(201, Timestamp.ofEpochMilli(201), KeyedEvent(AEvent(3)))))
            case 4 ⇒
              cBarrier.countDown()
              dBarrier.await(99, SECONDS) shouldBe true
              assert(request.after == 201)
              EventSeq.Empty(lastEventId = 201)
            case _ ⇒  // May be passed
              EventSeq.Empty(lastEventId = 201)
          }
        }

      def onEvents(stamped: Seq[Stamped[KeyedEvent[TestEvent]]]) =
        collector ++= stamped
    }

    val whenCompleted = fetcher.start()
    aBarrier.await(99, SECONDS) shouldBe true
    assert(collector == List(
      Stamped(101, Timestamp.ofEpochMilli(101), KeyedEvent(AEvent(1))),
      Stamped(102, Timestamp.ofEpochMilli(102), KeyedEvent(AEvent(2)))))
    bBarrier.countDown()
    cBarrier.await(99, SECONDS) shouldBe true
    assert(!whenCompleted.isCompleted)
    dBarrier.countDown()
    fetcher.close()
    whenCompleted await 3.s
    assert(collector == List(
      Stamped(101, Timestamp.ofEpochMilli(101), KeyedEvent(AEvent(1))),
      Stamped(102, Timestamp.ofEpochMilli(102), KeyedEvent(AEvent(2))),
      Stamped(201, Timestamp.ofEpochMilli(201), KeyedEvent(AEvent(3)))))
  }
}

object EventFetcherTest {
  private trait TestEvent extends NoKeyEvent
  private case class AEvent(number: Long) extends TestEvent
}
