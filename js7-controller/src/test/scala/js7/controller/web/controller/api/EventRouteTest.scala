package js7.controller.web.controller.api

import cats.effect.IO
import cats.effect.kernel.Deferred
import cats.effect.unsafe.IORuntime
import js7.base.circeutils.CirceUtils.RichCirceString
import js7.base.configutils.Configs.{HoconStringInterpolator, RichConfig}
import js7.base.monixlike.MonixLikeExtensions.scheduleOnce
import js7.base.problem.{Checked, Problem}
import js7.base.test.OurTestSuite
import js7.base.thread.CatsBlocking.syntax.*
import js7.base.thread.Futures.implicits.SuccessFuture
import js7.base.time.ScalaTime.*
import js7.base.time.Timestamp
import js7.base.utils.ByteSequenceToLinesStream
import js7.base.utils.ScalaUtils.syntax.RichEither
import js7.common.http.JsonStreamingSupport
import js7.common.http.JsonStreamingSupport.`application/x-ndjson`
import js7.common.http.PekkoHttpUtils.{RichHttpResponse, RichResponseEntity}
import js7.common.http.StreamingSupport.asFs2Stream
import js7.common.pekkohttp.PekkoHttpServerUtils.pathSegments
import js7.common.pekkoutils.ByteStrings.syntax.*
import js7.controller.web.controller.api.EventRouteTest.*
import js7.controller.web.controller.api.test.RouteTester
import js7.data.Problems.AckFromActiveClusterNodeProblem
import js7.data.event.{EventId, KeyedEvent, Stamped}
import js7.data.order.OrderEvent.{OrderAdded, OrderFinished}
import js7.data.order.{OrderEvent, OrderId}
import js7.data.workflow.WorkflowPath
import js7.journal.watch.{JournalEventWatch, SimpleEventCollector}
import js7.journal.web.EventDirectives
import org.apache.pekko.http.scaladsl.model.ContentType
import org.apache.pekko.http.scaladsl.model.MediaTypes.`application/json`
import org.apache.pekko.http.scaladsl.model.StatusCodes.{BadRequest, OK}
import org.apache.pekko.http.scaladsl.model.headers.Accept
import org.apache.pekko.http.scaladsl.testkit.RouteTestTimeout
import org.apache.pekko.util.ByteString
import scala.concurrent.duration.*
import scala.concurrent.duration.Deadline.now

/**
  * @author Joacim Zschimmer
  */
final class EventRouteTest extends OurTestSuite, RouteTester, EventRoute
{
  private implicit val timeout: FiniteDuration = 99.s
  private implicit val routeTestTimeout: RouteTestTimeout = RouteTestTimeout(timeout - 1.s)
  protected def whenShuttingDown = Deferred.unsafe
  protected def actorRefFactory = system

  private def scheduler = ioRuntime.scheduler
  private given IORuntime = ioRuntime

  private lazy val eventCollector = SimpleEventCollector[OrderEvent]()

  protected def eventWatch: JournalEventWatch = eventCollector.eventWatch

  override protected def config = config"""
    js7.web.chunk-size = 1MiB
    js7.web.client.prefetch = 0
    js7.web.server.prefetch = 0
    pekko.actor.default-dispatcher.fork-join-executor {
      parallelism-min = 1
      parallelism-factor = 0
      parallelism-max = 2
    }""" withFallback super.config

  private lazy val defaultDelay =
    config.finiteDuration("js7.web.server.services.event.streaming.delay").orThrow

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
    "/event?return=OrderEvent&timeout=0&after=0",
    "/event?timeout=0&after=0"))
  {
    s"$uri" in {
      assert(getEvents(uri) == TestEvents)
    }
  }

  "/event application/x-ndjson" in {
    Get("/event?after=0&limit=2") ~> Accept(`application/x-ndjson`) ~> route ~> check {
      if status != OK then fail(s"$status - ${responseEntity.toStrict(timeout).value}")
      assert(response.entity.contentType == ContentType(`application/x-ndjson`))
      assert(response.utf8String.await(99.s) ==
        """{"eventId":10,"timestamp":999,"Key":"1","TYPE":"OrderAdded","workflowId":{"path":"test","versionId":"VERSION"}}""" + '\n' +
        """{"eventId":20,"timestamp":999,"Key":"2","TYPE":"OrderAdded","workflowId":{"path":"test","versionId":"VERSION"}}""" + '\n')

      //implicit val x = JsonSeqStreamingSupport
      //implicit val y = CirceJsonSeqSupport
      //val stamped = responseAs[Source[Stamped[KeyedEvent[OrderEvent]], NotUsed]]
      //  .runFold(Vector.empty[Stamped[KeyedEvent[OrderEvent]]])(_ :+ _) await 99.s
      //assert(stamped == TestEvents)
    }
  }

  "/event application/x-ndjson with after=unknown fails" in {
    Get("/event?after=5") ~> Accept(`application/x-ndjson`) ~> route ~> check {
      assert(status == BadRequest)
      assert(response.utf8String.await(99.s) ==
        "EventSeqTorn: Requested EventId after=5/1970-01-01T00:00:00Z is not available. Oldest available EventId is 0/BeforeFirst\n")
    }
  }

  "Fetch EventIds only" - {
    "/event?onlyAcks=true&timeout=0 while isActiveNode" in {
      val stampedSeq = getEventIds("/event?onlyAcks=true&timeout=0")
      assert(stampedSeq == Left(AckFromActiveClusterNodeProblem))
    }

    "/event?onlyAcks=true&timeout=0" in {
      val wasActive = eventWatch.isActiveNode
      eventWatch.isActiveNode = false

      val stampedSeq = getEventIds("/event?onlyAcks=true&timeout=0")
      assert(stampedSeq == Right(Seq("180")))

      eventWatch.isActiveNode = wasActive
    }

    def getEventIds(uri: String): Checked[Seq[String]] =
      Get(uri) ~> Accept(`application/x-ndjson`, `application/json`) ~> route ~> check {
        if status != OK then
          responseEntity.toStrict(timeout).await(99.s)
            .asUtf8String.await(99.s)
            .parseJsonAs[Problem]
            .flatMap(Left(_))
        else
          Right(responseAs[ByteString].utf8String match {
            case "" => Vector.empty
            case string => string.split('\n').toVector
          })
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
      assert(getEvents("/event?after=180&timeout=0").isEmpty)
    }

    "/event?after=180 no more events, with timeout" in {
      val runningSince = now
      assert(getEvents("/event?after=180&timeout=0.2").isEmpty)
      assert(runningSince.elapsed >= 200.millis)
    }

    "/event DefaultDelay" in {
      val stamped = Stamped(EventId(190), OrderId("190") <-: OrderFinished())
      val runningSince = now
      scheduler.scheduleOnce(100.millis) {
        eventCollector.addStamped(stamped)
      }
      val stampedSeq = getEvents("/event?timeout=1&after=180")
      assert(stampedSeq == stamped :: Nil)
      assert(runningSince.elapsed >= 100.millis + defaultDelay)
    }

    "/event?delay=0 MinimumDelay" in {
      val stamped = Stamped(EventId(200), OrderId("200") <-: OrderFinished())
      val runningSince = now
      scheduler.scheduleOnce(100.millis) {
        eventCollector.addStamped(stamped)
      }
      val stampedSeq = getEvents("/event?delay=0&timeout=1&after=190")
      assert(stampedSeq == stamped :: Nil)
      assert(runningSince.elapsed >= 100.millis + EventDirectives.MinimumDelay)
    }

    "/event?delay=0.2" in {
      val stamped = Stamped(EventId(210), OrderId("210") <-: OrderFinished())
      val runningSince = now
      scheduler.scheduleOnce(100.millis) {
        eventCollector.addStamped(stamped)
      }
      val stampedSeq = getEvents("/event?delay=0.2&timeout=1&after=200")
      assert(stampedSeq == stamped :: Nil)
      assert(runningSince.elapsed >= 100.millis + 200.millis)
    }

    "After truncated journal snapshot" in pending  // TODO Test torn event stream
  }

  private def getEvents(uri: String): Seq[Stamped[KeyedEvent[OrderEvent]]] =
    Get(uri) ~> Accept(`application/x-ndjson`) ~> route ~> check {
      if status != OK then fail(s"$status - ${responseEntity.toStrict(timeout).value}")
      response.entity.withoutSizeLimit.dataBytes
        .asFs2Stream()
        .flatMap(ByteSequenceToLinesStream())
        .map(_.parseJsonAs[Stamped[KeyedEvent[OrderEvent]]].orThrow)
        .compile.toList
        .await(99.s)
    }
}


object EventRouteTest
{
  private val TestEvents = for i <- 1 to 18 yield
    Stamped(EventId(10 * i), Timestamp.ofEpochMilli(999),
      OrderId(i.toString) <-: OrderAdded(WorkflowPath("test") ~ "VERSION"))
}
