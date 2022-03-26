package js7.controller.web.controller.api

import akka.http.scaladsl.model.ContentType
import akka.http.scaladsl.model.MediaTypes.`application/json`
import akka.http.scaladsl.model.StatusCodes.{BadRequest, OK}
import akka.http.scaladsl.model.headers.Accept
import akka.http.scaladsl.testkit.RouteTestTimeout
import akka.util.ByteString
import js7.base.configutils.Configs.HoconStringInterpolator
import js7.base.thread.MonixBlocking.syntax._
import js7.base.time.ScalaTime._
import js7.base.time.Timestamp
import js7.common.akkahttp.AkkaHttpServerUtils.pathSegments
import js7.common.akkahttp.CirceJsonSupport.jsonUnmarshaller
import js7.common.http.AkkaHttpUtils.RichHttpResponse
import js7.common.http.JsonStreamingSupport.`application/x-ndjson`
import js7.controller.web.controller.api.EventRouteTest._
import js7.controller.web.controller.api.test.RouteTester
import js7.data.event.{EventId, EventSeq, KeyedEvent, Stamped, TearableEventSeq}
import js7.data.order.OrderEvent.{OrderAdded, OrderFinished}
import js7.data.order.{OrderEvent, OrderId}
import js7.data.workflow.WorkflowPath
import js7.journal.watch.SimpleEventCollector
import js7.journal.web.EventDirectives
import monix.execution.Scheduler
import org.scalatest.freespec.AnyFreeSpec
import scala.concurrent.Future
import scala.concurrent.duration.Deadline.now
import scala.concurrent.duration._

/**
  * @author Joacim Zschimmer
  */
final class EventRouteTest extends AnyFreeSpec with RouteTester with EventRoute
{
  private implicit val timeout = 99.s
  private implicit val routeTestTimeout = RouteTestTimeout(timeout - 1.s)
  protected def whenShuttingDown = Future.never
  protected def actorRefFactory = system
  protected implicit def scheduler: Scheduler = Scheduler.global
  private lazy val eventCollector = SimpleEventCollector[OrderEvent]()
  protected def eventWatch = eventCollector.eventWatch

  override protected def config = config"""
    js7.web.chunk-size = 1MiB
    akka.actor.default-dispatcher.fork-join-executor {
      parallelism-min = 1
      parallelism-factor = 0
      parallelism-max = 2
    }""" withFallback super.config

  private val route = pathSegments("event")(eventRoute)

  override def beforeAll() = {
    super.beforeAll()
    TestEvents foreach eventCollector.addStamped
  }

  override def afterAll() = {
    eventCollector.close()
    super.afterAll()
  }

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

  "/event application/x-ndjson" in {
    Get(s"/event?after=0&limit=2") ~> Accept(`application/x-ndjson`) ~> route ~> check {
      if (status != OK) fail(s"$status - ${responseEntity.toStrict(timeout).value}")
      assert(response.entity.contentType == ContentType(`application/x-ndjson`))
      assert(response.utf8String.await(99.s) ==
        s"""{"eventId":10,"timestamp":999,"Key":"1","TYPE":"OrderAdded","workflowId":{"path":"test","versionId":"VERSION"}}""" + '\n' +
        s"""{"eventId":20,"timestamp":999,"Key":"2","TYPE":"OrderAdded","workflowId":{"path":"test","versionId":"VERSION"}}""" + '\n')

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
      assert(response.utf8String.await(99.s) ==
        s"EventSeqTorn: Requested EventId after=5/1970-01-01T00:00:00.000Z-005 is not available. Oldest available EventId is 0/BeforeFirst\n")
    }
  }

  "Fetch EventIds only" - {
    "/event?onlyAcks=true&timeout=0" in {
      val stampedSeq = getEventIds("/event?onlyAcks=true&timeout=0")
      assert(stampedSeq == Seq(180L))
    }

    def getEventIds(uri: String): Seq[EventId] =
      Get(uri) ~> Accept(`application/x-ndjson`) ~> route ~> check {
        if (status != OK) fail(s"$status - ${responseEntity.toStrict(timeout).value}")
        responseAs[ByteString].utf8String match {
          case "" => Vector.empty
          case string => string.split('\n').map(java.lang.Long.parseLong).toVector
        }
      }
  }

  "Fetch events with repeated GET requests" - {
    "/event?limit=3&after=30 continue" in {
      val stampedSeq = getEvents("/event?limit=3&after=30")
      assert(stampedSeq.head.eventId == 40)
      assert(stampedSeq.last.eventId == 60)
    }

    "/event?limit=3&after=60 continue" in {
      val stampedSeq = getEvents("/event?limit=3&after=60")
      assert(stampedSeq.head.eventId == 70)
      assert(stampedSeq.last.eventId == 90)
    }

    "/event?limit=1&after=70 rewind in last chunk" in {
      val stampedSeq = getEvents("/event?limit=3&after=70")
      assert(stampedSeq.head.eventId ==  80)
      assert(stampedSeq.last.eventId == 100)
    }

    "/event?limit=3&after=80 continue" in {
      val stampedSeq = getEvents("/event?limit=3&after=80")
      assert(stampedSeq.head.eventId ==  90)
      assert(stampedSeq.last.eventId == 110)
    }

    "/event?limit=3&after=60 rewind to oldest" in {
      val stampedSeq = getEvents("/event?limit=3&after=60")
      assert(stampedSeq.head.eventId == 70)
      assert(stampedSeq.last.eventId == 90)
    }

    "/event?limit=3&after=150 skip some events" in {
      val runningSince = now
      val stampedSeq = getEvents("/event?delay=99&limit=3&after=150")
      assert(runningSince.elapsed < 4.s/*Sometimes, 1s is too short???*/)  // Events must have been returned immediately
      assert(stampedSeq.head.eventId == 160)
      assert(stampedSeq.last.eventId == 180)
    }

    "/event?after=180 no more events" in {
      assert(getEventSeq("/event?after=180") == EventSeq.Empty(180L))
    }

    "/event?after=180 no more events, with timeout" in {
      val runningSince = now
      assert(getEventSeq("/event?after=180&timeout=0.2") == EventSeq.Empty(180L))
      assert(runningSince.elapsed >= 200.millis)
    }

    "/event DefaultDelay" in {
      val stamped = Stamped(EventId(190), OrderId("190") <-: OrderFinished)
      val runningSince = now
      scheduler.scheduleOnce(100.millis) {
        eventCollector.addStamped(stamped)
      }
      val stampedSeq = getEvents("/event?timeout=30&after=180")
      assert(stampedSeq == stamped :: Nil)
      assert(runningSince.elapsed >= 100.millis + EventDirectives.DefaultDelay)
    }

    "/event?delay=0 MinimumDelay" in {
      val stamped = Stamped(EventId(200), OrderId("200") <-: OrderFinished)
      val runningSince = now
      scheduler.scheduleOnce(100.millis) {
        eventCollector.addStamped(stamped)
      }
      val stampedSeq = getEvents("/event?delay=0&timeout=30&after=190")
      assert(stampedSeq == stamped :: Nil)
      assert(runningSince.elapsed >= 100.millis + EventDirectives.MinimumDelay)
    }

    "/event?delay=0.2" in {
      val stamped = Stamped(EventId(210), OrderId("210") <-: OrderFinished)
      val runningSince = now
      scheduler.scheduleOnce(100.millis) {
        eventCollector.addStamped(stamped)
      }
      val stampedSeq = getEvents("/event?delay=0.2&timeout=30&after=200")
      assert(stampedSeq == stamped :: Nil)
      assert(runningSince.elapsed >= 100.millis + 200.millis)
    }

    "After truncated journal snapshot" in pending  // TODO Test torn event stream
  }

  private def getEvents(uri: String): Seq[Stamped[KeyedEvent[OrderEvent]]] =
    getEventSeq(uri) match {
      case EventSeq.NonEmpty(stampedSeq) =>
        assert(stampedSeq.nonEmpty)
        stampedSeq

      case x => fail(s"Unexpected response: $x")
    }

  private def getEventSeq(uri: String): TearableEventSeq[Seq, KeyedEvent[OrderEvent]] =
    Get(uri) ~> Accept(`application/json`) ~> route ~> check {
      if (status != OK) fail(s"$status - ${responseEntity.toStrict(timeout).value}")
      responseAs[TearableEventSeq[Seq, KeyedEvent[OrderEvent]]]
    }
}

object EventRouteTest
{
  private val TestEvents = for (i <- 1 to 18) yield
    Stamped(EventId(10 * i), Timestamp.ofEpochMilli(999),
      OrderId(i.toString) <-: OrderAdded(WorkflowPath("test") ~ "VERSION"))
}
