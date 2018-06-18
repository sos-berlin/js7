package com.sos.jobscheduler.master.agent

import com.sos.jobscheduler.base.time.Timestamp
import com.sos.jobscheduler.common.scalautil.Futures.implicits._
import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.data.event.{EventRequest, EventSeq, KeyedEvent, NoKeyEvent, Stamped}
import com.sos.jobscheduler.master.agent.EventFetcherTest._
import com.typesafe.config.ConfigFactory
import java.util.concurrent.CountDownLatch
import monix.execution.Scheduler
import org.scalatest.FreeSpec
import scala.collection.JavaConverters._
import scala.collection.immutable.Seq
import scala.collection.mutable
import scala.concurrent.Future

/**
  * @author Joacim Zschimmer
  */
final class EventFetcherTest extends FreeSpec {

  "test" in {
    val collector = mutable.Buffer[Stamped[KeyedEvent[TestEvent]]]()
    val aBarrier, bBarrier, cBarrier, dBarrier = new CountDownLatch(1)
    implicit val scheduler = Scheduler.global
    val fetcher = new EventFetcher[TestEvent](after = 100) {
      protected val config = ConfigFactory.parseMap(Map("jobscheduler.master.agent-driver.event-fetcher.delay" → "0ms").asJava)
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
              bBarrier.await()
              assert(request.after == 102)
              EventSeq.Empty(lastEventId = 200)
            case 3 ⇒
              assert(request.after == 200)
              EventSeq.NonEmpty(List(
                Stamped(201, Timestamp.ofEpochMilli(201), KeyedEvent(AEvent(3)))))
            case 4 ⇒
              cBarrier.countDown()
              dBarrier.await()
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
    aBarrier.await()
    assert(collector == List(
      Stamped(101, Timestamp.ofEpochMilli(101), KeyedEvent(AEvent(1))),
      Stamped(102, Timestamp.ofEpochMilli(102), KeyedEvent(AEvent(2)))))
    bBarrier.countDown()
    cBarrier.await()
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
