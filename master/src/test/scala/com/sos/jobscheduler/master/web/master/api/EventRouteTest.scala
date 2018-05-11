package com.sos.jobscheduler.master.web.master.api

import akka.http.scaladsl.model.ContentType
import akka.http.scaladsl.model.MediaTypes.`application/json`
import akka.http.scaladsl.model.StatusCodes.OK
import akka.http.scaladsl.model.headers.Accept
import akka.http.scaladsl.testkit.ScalatestRouteTest
import com.google.common.base.Ascii
import com.sos.jobscheduler.base.time.Timestamp
import com.sos.jobscheduler.common.akkahttp.AkkaHttpServerUtils.pathSegments
import com.sos.jobscheduler.common.event.collector.EventCollector
import com.sos.jobscheduler.common.http.AkkaHttpUtils.RichHttpResponse
import com.sos.jobscheduler.common.http.CirceJsonSeqSupport.`application/json-seq`
import com.sos.jobscheduler.common.http.CirceJsonSupport._
import com.sos.jobscheduler.common.scalautil.Futures.implicits._
import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.common.time.timer.TimerService
import com.sos.jobscheduler.data.event.{EventId, EventSeq, KeyedEvent, Stamped, TearableEventSeq}
import com.sos.jobscheduler.data.order.OrderEvent.OrderAdded
import com.sos.jobscheduler.data.order.{OrderEvent, OrderId, Payload}
import com.sos.jobscheduler.data.workflow.WorkflowPath
import com.sos.jobscheduler.master.web.master.api.EventRouteTest._
import monix.execution.Scheduler
import org.scalatest.FreeSpec
import scala.collection.immutable.Seq
import scala.concurrent.duration._

/**
  * @author Joacim Zschimmer
  */
final class EventRouteTest extends FreeSpec with ScalatestRouteTest with EventRoute {

  private implicit val timeout = 9.seconds
  private implicit val timerService = new TimerService(idleTimeout = Some(1.s))
  protected val eventReader = new EventCollector.ForTest
  protected implicit def scheduler = Scheduler.global

  TestEvents foreach eventReader.addStamped

  private def route = pathSegments("event")(eventRoute)

  for (uri ← List(
    "/event?return=OrderEvent&timeout=60&after=0",
    "/event?timeout=60&after=0"))
  {
    s"$uri" in {
      Get(uri) ~> Accept(`application/json`) ~> route ~> check {
        if (status != OK) fail(s"$status - ${responseEntity.toStrict(timeout).value}")
        val EventSeq.NonEmpty(stampeds) = responseAs[TearableEventSeq[Seq, KeyedEvent[OrderEvent]]]
        assert(stampeds == TestEvents)
      }
    }
  }

  "/event application/json-seq" in {
    Get(s"/event?after=0&limit=2") ~> Accept(`application/json-seq`) ~> route ~> check {
      if (status != OK) fail(s"$status - ${responseEntity.toStrict(timeout).value}")
      assert(response.entity.contentType == ContentType(`application/json-seq`))
      val rs = Ascii.RS.toChar
      val lf = Ascii.LF.toChar
      assert(response.utf8StringFuture.await(99.s) ==
        s"""$rs{"eventId":1000,"timestamp":999,"key":"1","TYPE":"OrderAdded","workflowId":{"path":"/test","versionId":"VERSION"}}$lf""" +
        s"""$rs{"eventId":2000,"timestamp":999,"key":"2","TYPE":"OrderAdded","workflowId":{"path":"/test","versionId":"VERSION"}}$lf""")

      //implicit val x = JsonSeqStreamSupport
      //implicit val y = CirceJsonSeqSupport
      //val stampeds = responseAs[Source[Stamped[KeyedEvent[OrderEvent]], NotUsed]]
      //  .runFold(Vector.empty[Stamped[KeyedEvent[OrderEvent]]])(_ :+ _) await 99.s
      //assert(stampeds == TestEvents)
    }
  }
}

object EventRouteTest
{
  private val TestEvents = for (i ← 1 to 6) yield
    Stamped(EventId(1000 * i), Timestamp.ofEpochMilli(999),
      OrderId(i.toString) <-: OrderAdded(WorkflowPath("/test") % "VERSION", None, Payload.empty))
}
