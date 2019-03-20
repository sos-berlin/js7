package com.sos.jobscheduler.master.web.master.api

import akka.http.scaladsl.model.ContentType
import akka.http.scaladsl.model.MediaTypes.{`application/json`, `text/event-stream`}
import akka.http.scaladsl.model.StatusCodes.{BadRequest, OK}
import akka.http.scaladsl.model.headers.{Accept, `Last-Event-ID`}
import akka.http.scaladsl.testkit.RouteTestTimeout
import com.google.common.base.Ascii
import com.sos.jobscheduler.base.circeutils.CirceUtils.RichCirceString
import com.sos.jobscheduler.base.problem.Problem
import com.sos.jobscheduler.base.time.Timestamp
import com.sos.jobscheduler.base.time.Timestamp.now
import com.sos.jobscheduler.base.utils.ScalaUtils.RichEither
import com.sos.jobscheduler.common.akkahttp.AkkaHttpServerUtils.pathSegments
import com.sos.jobscheduler.common.akkahttp.JsonStreamingSupport.{`application/json-seq`, `application/x-ndjson`}
import com.sos.jobscheduler.common.event.collector.{EventCollector, EventDirectives}
import com.sos.jobscheduler.common.http.AkkaHttpUtils.RichHttpResponse
import com.sos.jobscheduler.common.http.CirceJsonSupport._
import com.sos.jobscheduler.common.scalautil.Futures.implicits._
import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.core.message.ProblemCodeMessages
import com.sos.jobscheduler.data.event.{EventId, EventSeq, KeyedEvent, Stamped, TearableEventSeq}
import com.sos.jobscheduler.data.order.OrderEvent.{OrderAdded, OrderFinished}
import com.sos.jobscheduler.data.order.{OrderEvent, OrderId}
import com.sos.jobscheduler.data.workflow.WorkflowPath
import com.sos.jobscheduler.master.web.master.api.EventRouteTest._
import com.sos.jobscheduler.master.web.master.api.test.RouteTester
import monix.execution.Scheduler
import org.scalatest.FreeSpec
import scala.collection.immutable.Seq
import scala.concurrent.duration._

/**
  * @author Joacim Zschimmer
  */
final class EventRouteTest extends FreeSpec with RouteTester with EventRoute
{
  ProblemCodeMessages.initialize()

  private implicit val timeout = 9.seconds
  private implicit val routeTestTimeout = RouteTestTimeout(timeout)
  protected val eventWatch = new EventCollector.ForTest()(Scheduler.global)
  protected implicit def scheduler: Scheduler = Scheduler.global

  TestEvents foreach eventWatch.addStamped

  private def route = pathSegments("event")(eventRoute)

  for (uri <- List(
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
      val RS = Ascii.RS.toChar
      assert(response.utf8StringFuture.await(99.s) ==
        s"""$RS{"eventId":10,"timestamp":999,"key":"1","TYPE":"OrderAdded","workflowId":{"path":"/test","versionId":"VERSION"}}""" + '\n' +
        s"""$RS{"eventId":20,"timestamp":999,"key":"2","TYPE":"OrderAdded","workflowId":{"path":"/test","versionId":"VERSION"}}""" + '\n')

      //implicit val x = JsonSeqStreamingSupport
      //implicit val y = CirceJsonSeqSupport
      //val stamped = responseAs[Source[Stamped[KeyedEvent[OrderEvent]], NotUsed]]
      //  .runFold(Vector.empty[Stamped[KeyedEvent[OrderEvent]]])(_ :+ _) await 99.s
      //assert(stamped == TestEvents)
    }
  }

  "/event application/x-ndjson" in {
    Get(s"/event?after=0&limit=2") ~> Accept(`application/x-ndjson`) ~> route ~> check {
      if (status != OK) fail(s"$status - ${responseEntity.toStrict(timeout).value}")
      assert(response.entity.contentType == ContentType(`application/x-ndjson`))
      assert(response.utf8StringFuture.await(99.s) ==
        s"""{"eventId":10,"timestamp":999,"key":"1","TYPE":"OrderAdded","workflowId":{"path":"/test","versionId":"VERSION"}}""" + '\n' +
        s"""{"eventId":20,"timestamp":999,"key":"2","TYPE":"OrderAdded","workflowId":{"path":"/test","versionId":"VERSION"}}""" + '\n')

      //implicit val x = JsonSeqStreamingSupport
      //implicit val y = CirceJsonSeqSupport
      //val stamped = responseAs[Source[Stamped[KeyedEvent[OrderEvent]], NotUsed]]
      //  .runFold(Vector.empty[Stamped[KeyedEvent[OrderEvent]]])(_ :+ _) await 99.s
      //assert(stamped == TestEvents)
    }
  }

  "/event application/x-ndjson with after=unknown fails" in {
    Get(s"/event?after=5") ~> Accept(`application/x-ndjson`) ~> route ~> check {
      assert(status == BadRequest)
      assert(response.utf8StringFuture.await(99.s) == s"EventSeqTorn: Requested EventId after=5 is not available. Oldest available EventId is 0\n")
    }
  }

  "Fetch events with repeated GET requests" - {  // Similar to FatEventRouteTest
    "/event?limit=3&after=30 continue" in {
      val stampeds = getEvents("/event?limit=3&after=30")
      assert(stampeds.head.eventId == 40)
      assert(stampeds.last.eventId == 60)
    }

    "/event?limit=3&after=60 continue" in {
      val stampeds = getEvents("/event?limit=3&after=60")
      assert(stampeds.head.eventId == 70)
      assert(stampeds.last.eventId == 90)
    }

    "/event?limit=1&after=70 rewind in last chunk" in {
      val stampeds = getEvents("/event?limit=3&after=70")
      assert(stampeds.head.eventId ==  80)
      assert(stampeds.last.eventId == 100)
    }

    "/event?limit=3&after=80 continue" in {
      val stampeds = getEvents("/event?limit=3&after=80")
      assert(stampeds.head.eventId ==  90)
      assert(stampeds.last.eventId == 110)
    }

    "/event?limit=3&after=60 rewind to oldest" in {
      val stampeds = getEvents("/event?limit=3&after=60")
      assert(stampeds.head.eventId == 70)
      assert(stampeds.last.eventId == 90)
    }

    "/event?limit=3&after=150 skip some events" in {
      val t = now
      val stampeds = getEvents("/event?limit=3&after=150")
      assert(now < t + EventDirectives.MinimumDelay)
      assert(stampeds.head.eventId == 160)
      assert(stampeds.last.eventId == 180)
    }

    "/event?after=180 no more events" in {
      assert(getEventSeq("/event?after=180") == EventSeq.Empty(180))
    }

    "/event?after=180 no more events, with timeout" in {
      val t = now
      assert(getEventSeq("/event?after=180&timeout=0.2") == EventSeq.Empty(180))
      assert(now - t >= 200.millis)
    }

    "/event DefaultDelay" in {
      val stamped = Stamped(EventId(190), OrderId("190") <-: OrderFinished)
      val t = now
      scheduler.scheduleOnce(100.millis) {
        eventWatch.addStamped(stamped)
      }
      val stampeds = getEvents("/event?timeout=30&after=180")
      assert(stampeds == stamped :: Nil)
      assert(now - t >= 100.millis + EventDirectives.DefaultDelay)
    }

    "/event?delay=0 MinimumDelay" in {
      val stamped = Stamped(EventId(200), OrderId("200") <-: OrderFinished)
      val t = now
      scheduler.scheduleOnce(100.millis) {
        eventWatch.addStamped(stamped)
      }
      val stampeds = getEvents("/event?delay=0&timeout=30&after=190")
      assert(stampeds == stamped :: Nil)
      assert(now - t >= 100.millis + EventDirectives.MinimumDelay)
    }

    "/event?delay=0.2" in {
      val stamped = Stamped(EventId(210), OrderId("210") <-: OrderFinished)
      val t = now
      scheduler.scheduleOnce(100.millis) {
        eventWatch.addStamped(stamped)
      }
      val stampeds = getEvents("/event?delay=0.2&timeout=30&after=200")
      assert(stampeds == stamped :: Nil)
      assert(now - t >= 100.millis + 200.millis)
    }

    "After truncated journal snapshot" in pending  // TODO Test torn event stream
  }

  private def getEvents(uri: String): Seq[Stamped[KeyedEvent[OrderEvent]]] =
    getEventSeq(uri) match {
      case EventSeq.NonEmpty(stampeds) =>
        assert(stampeds.nonEmpty)
        stampeds

      case x => fail(s"Unexpected response: $x")
    }

  private def getEventSeq(uri: String): TearableEventSeq[Seq, KeyedEvent[OrderEvent]] =
    Get(uri) ~> Accept(`application/json`) ~> route ~> check {
      if (status != OK) fail(s"$status - ${responseEntity.toStrict(timeout).value}")
      responseAs[TearableEventSeq[Seq, KeyedEvent[OrderEvent]]]
    }

  "Server-sent events" - {
    "/event?after=0" in {
      Get(s"/event?after=0&limit=2") ~> Accept(`text/event-stream`) ~> route ~> check {
        if (status != OK) fail(s"$status - ${responseEntity.toStrict(timeout).value}")
        assert(response.entity.contentType == ContentType(`text/event-stream`))
        assert(response.utf8StringFuture.await(99.s) ==
          """data:{"eventId":10,"timestamp":999,"key":"1","TYPE":"OrderAdded","workflowId":{"path":"/test","versionId":"VERSION"}}
            |id:10
            |
            |data:{"eventId":20,"timestamp":999,"key":"2","TYPE":"OrderAdded","workflowId":{"path":"/test","versionId":"VERSION"}}
            |id:20
            |
            |""".stripMargin)
      }
    }

    "/event?after=0, retry with Last-Event-Id" in {
      Get(s"/event?after=0&limit=2") ~> Accept(`text/event-stream`) ~> `Last-Event-ID`("20") ~> route ~> check {
        if (status != OK) fail(s"$status - ${responseEntity.toStrict(timeout).value}")
        assert(response.entity.contentType == ContentType(`text/event-stream`))
        assert(response.utf8StringFuture.await(99.s) ==
          """data:{"eventId":30,"timestamp":999,"key":"3","TYPE":"OrderAdded","workflowId":{"path":"/test","versionId":"VERSION"}}
            |id:30
            |
            |data:{"eventId":40,"timestamp":999,"key":"4","TYPE":"OrderAdded","workflowId":{"path":"/test","versionId":"VERSION"}}
            |id:40
            |
            |""".stripMargin)
      }
    }

    "/event?v=XXX&after=0, buildId changed" in {
      Get(s"/event?v=XXX&after=0") ~> Accept(`text/event-stream`) ~> `Last-Event-ID`("20") ~> route ~> check {
        if (status != OK) fail(s"$status - ${responseEntity.toStrict(timeout).value}")
        assert(response.entity.contentType == ContentType(`text/event-stream`))
        val string = response.utf8StringFuture.await(99.s)
        assert(string ==
          """data:{"TYPE":"Problem","message":"BUILD-CHANGED"}
            |
            |""".stripMargin)
        assert(string.drop(5).parseJsonOrThrow.as[Problem].orThrow.toString == "BUILD-CHANGED")
      }
    }
  }
}

object EventRouteTest
{
  private val TestEvents = for (i <- 1 to 18) yield
    Stamped(EventId(10 * i), Timestamp.ofEpochMilli(999),
      OrderId(i.toString) <-: OrderAdded(WorkflowPath("/test") ~ "VERSION", None, Map.empty))
}
