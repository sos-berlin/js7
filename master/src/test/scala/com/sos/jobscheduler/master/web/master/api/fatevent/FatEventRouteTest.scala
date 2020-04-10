package com.sos.jobscheduler.master.web.master.api.fatevent

import akka.http.scaladsl.model.MediaTypes.`application/json`
import akka.http.scaladsl.model.StatusCodes.{OK, TooManyRequests}
import akka.http.scaladsl.model.headers.Accept
import akka.http.scaladsl.testkit.RouteTestTimeout
import com.sos.jobscheduler.base.time.ScalaTime._
import com.sos.jobscheduler.base.time.Timestamp
import com.sos.jobscheduler.base.utils.CloseableIterator
import com.sos.jobscheduler.base.web.Uri
import com.sos.jobscheduler.common.akkahttp.AkkaHttpServerUtils.pathSegments
import com.sos.jobscheduler.common.event.collector.{EventCollector, EventDirectives}
import com.sos.jobscheduler.common.http.CirceJsonSupport._
import com.sos.jobscheduler.common.scalautil.FileUtils.deleteDirectoryRecursively
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.core.crypt.silly.SillySigner
import com.sos.jobscheduler.core.event.journal.data.JournalHeader
import com.sos.jobscheduler.core.filebased.FileBasedSigner
import com.sos.jobscheduler.data.agent.{AgentRef, AgentRefPath}
import com.sos.jobscheduler.data.event.KeyedEvent.NoKey
import com.sos.jobscheduler.data.event.{AnyKeyedEvent, EventId, EventSeq, JournalId, KeyedEvent, Stamped, TearableEventSeq}
import com.sos.jobscheduler.data.fatevent.FatEvent
import com.sos.jobscheduler.data.fatevent.OrderFatEvent.{OrderAddedFat, OrderProcessedFat, OrderProcessingStartedFat, OrderStdoutWrittenFat}
import com.sos.jobscheduler.data.filebased.RepoEvent.{FileBasedAdded, VersionAdded}
import com.sos.jobscheduler.data.filebased.VersionId
import com.sos.jobscheduler.data.job.ExecutablePath
import com.sos.jobscheduler.data.master.{MasterFileBaseds, MasterId}
import com.sos.jobscheduler.data.order.OrderEvent.{OrderAdded, OrderAttachable, OrderDetachable, OrderMoved, OrderProcessed, OrderProcessingStarted, OrderStarted, OrderStdoutWritten, OrderTransferredToAgent}
import com.sos.jobscheduler.data.order.{OrderId, Outcome}
import com.sos.jobscheduler.data.workflow.instructions.Execute
import com.sos.jobscheduler.data.workflow.instructions.executable.WorkflowJob
import com.sos.jobscheduler.data.workflow.position.Position
import com.sos.jobscheduler.data.workflow.{Workflow, WorkflowPath}
import com.sos.jobscheduler.master.configuration.MasterConfiguration
import com.sos.jobscheduler.master.data.MasterSnapshots.MasterMetaState
import com.sos.jobscheduler.master.web.master.api.fatevent.FatEventRouteTest._
import com.sos.jobscheduler.master.web.master.api.test.RouteTester
import java.nio.file.Files
import java.util.UUID.randomUUID
import monix.execution.Scheduler
import org.scalatest.Args
import scala.annotation.tailrec
import scala.concurrent.duration.Deadline.now
import scala.concurrent.duration._
import scala.util.{Failure, Success}
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class FatEventRouteTest extends AnyFreeSpec with RouteTester with FatEventRoute
{
  private lazy val configAndDataDirectory = Files.createTempDirectory("FatEventRouteTest-")
  protected val masterConfiguration = MasterConfiguration.forTest(configAndDataDirectory)
  protected def isShuttingDown = false

  override def afterAll(): Unit = {
    deleteDirectoryRecursively(configAndDataDirectory)
    super.afterAll()
  }

  override protected def runTest(testName: String, args: Args) = {
    logger.debug("-" * 50)
    logger.debug(s"""Test "$testName"""")
    val status = super.runTest(testName, args)
    status.whenCompleted {
      case Success(true) =>
      case Success(false) => logger.warn(s"""Test "$testName" FAILED""")
      case Failure(t) => logger.warn(s"""Test "$testName" FAILED: $t""")
    }
    status
  }

  private implicit val timeout = 99.seconds
  private implicit val routeTestTimeout = RouteTestTimeout(timeout)
  protected val eventWatch = new TestEventWatch
  protected implicit def scheduler: Scheduler = Scheduler.global

  InitialEvents foreach eventWatch.addStamped
  TestEvents.flatten foreach eventWatch.addStamped

  private def route = pathSegments("fatEvent")(fatEventRoute)

  for (uri <- List(
    "/fatEvent?return=OrderFatEvent&timeout=60&after=0",
    "/fatEvent?after=0"))
  {
    s"$uri" in {
      assert(getFatEvents(uri) == TestFatEvents)
    }
  }

  "Fetch events with repeated GET requests" - {  // Similar to EventRouteTest
    "/fatEvent?limit=3&after=0 start" in {
      assert(getFatEvents("/fatEvent?limit=3&after=0") == fatEventsAfter(0).take(3))
      assert(eventWatch.lastEventsAfter == 0)
    }

    "/fatEvent?limit=3&after=30 continue" in {
      val stampeds = getFatEvents("/fatEvent?limit=3&after=30")
      assert(stampeds == fatEventsAfter(30).take(3))
      assert(stampeds.head.eventId == 40)
      assert(stampeds.last.eventId == 60)
      assert(eventWatch.lastEventsAfter == 30)
    }

    "/fatEvent?limit=3&after=60 continue" in {
      val stampeds = getFatEvents("/fatEvent?limit=3&after=60")
      assert(stampeds == fatEventsAfter(60).take(3))
      assert(stampeds.head.eventId == 70)
      assert(stampeds.last.eventId == 90)
      assert(eventWatch.lastEventsAfter == 60)
    }

    "/fatEvent?limit=1&after=70 rewind in last read chunk" in {
      val stampeds = getFatEvents("/fatEvent?limit=3&after=70")
      assert(stampeds == fatEventsAfter(70).take(3))
      assert(stampeds.head.eventId ==  80)
      assert(stampeds.last.eventId == 100)
      assert(eventWatch.lastEventsAfter == 60)  // Rewound to lastRequested
    }

    "/fatEvent?limit=3&after=80 continue" in {
      val stampeds = getFatEvents("/fatEvent?limit=3&after=80")
      assert(stampeds == fatEventsAfter(80).take(3))
      assert(stampeds.head.eventId ==  90)
      assert(stampeds.last.eventId == 110)
      assert(eventWatch.lastEventsAfter == 70)  // Rewound to lastRequested
    }

    "/fatEvent?limit=3&after=60 rewind to oldest" in {
      val stampeds = getFatEvents("/fatEvent?limit=3&after=60")
      assert(stampeds == fatEventsAfter(60).take(3))
      assert(stampeds.head.eventId == 70)
      assert(stampeds.last.eventId == 90)
      assert(eventWatch.lastEventsAfter == eventWatch.tornEventId)  // Rewound to tornEventId == BeforeFirst (no snapshot taken)
      assert(eventWatch.lastEventsAfter == EventId.BeforeFirst)
    }

    "/fatEvent?limit=3&after=150 skip some events" in {
      val stampeds = getFatEvents("/fatEvent?limit=3&after=150")
      assert(stampeds == fatEventsAfter(150).take(3))
      assert(stampeds.head.eventId == 160)
      assert(stampeds.last.eventId == 180)
      assert(eventWatch.lastEventsAfter == 90)  // Skipped until after 150
    }

    "/fatEvent?after=180 no more events" in {
      assert(getFatEventSeq("/fatEvent?after=180") == EventSeq.Empty(182))  // One OrderEvent processed, not yet resulting in an OrderFatEvent
    }

    "/fatEvent?after=180, intermediate event added" in {
      eventWatch.addStamped(Stamped(EventId(190), OrderId("10") <-: OrderStarted))
      eventWatch.addStamped(Stamped(EventId(191), OrderId("10") <-: OrderProcessingStarted))
      eventWatch.addStamped(Stamped(EventId(192), OrderId("10") <-: OrderProcessed(Outcome.succeeded)))
      eventWatch.addStamped(Stamped(EventId(193), OrderId("10") <-: OrderMoved(Position(1))))

      assert(getFatEventSeq("/fatEvent?after=180") == EventSeq.NonEmpty(List(
        Stamped(191, OrderId("10") <-: OrderProcessingStartedFat(TestWorkflow.id /: Position(0), TestAgentRefId.path, Uri("http://127.0.0.1:0"), None, Map.empty)),
        Stamped(192, OrderId("10") <-: OrderProcessedFat(Outcome.succeeded, Map.empty)))))

      eventWatch.addStamped(Stamped(EventId(200), OrderId("10") <-: OrderDetachable))  // Does not yield an OrderFatEvent
      assert(getFatEventSeq("/fatEvent?after=193") == EventSeq.Empty(200))
      assert(getFatEventSeq("/fatEvent?after=194") == EventSeq.Empty(200))
      assert(getFatEventSeq("/fatEvent?after=200") == EventSeq.Empty(200))
      assert(getFatEventSeq("/fatEvent?after=201") == EventSeq.Empty(200))
    }

    "/fatEvent?after=193, intermediate event added (OrderDetached), with timeout" in {
      val runningSince = now
      assert(getFatEventSeq("/fatEvent?after=193&timeout=0.1") == EventSeq.Empty(200))
      assert(runningSince.elapsed >= 100.millis)
    }

    "/fatEvent?after=200, OrderFinished added" in {
      val runningSince = now
      scheduler.scheduleOnce(100.millis) {
        eventWatch.addStamped(Stamped(EventId(201), OrderId("10") <-: OrderStdoutWritten("1")))
      }
      assert(getFatEventSeq("/fatEvent?timeout=30&after=200") == EventSeq.NonEmpty(
        Stamped(201, OrderId("10") <-: OrderStdoutWrittenFat("1")):: Nil))
      assert(runningSince.elapsed >= 100.millis + EventDirectives.DefaultDelay, "(100ms + DefaultDelay)")
    }

    "/fatEvent?after=201, with delay" in {
      val runningSince = now
      scheduler.scheduleOnce(100.millis) {
        eventWatch.addStamped(Stamped(EventId(202), OrderId("10") <-: OrderStdoutWritten("2")))
      }
      assert(getFatEventSeq("/fatEvent?delay=0.1&timeout=30&after=201") == EventSeq.NonEmpty(
        Stamped(202, OrderId("10") <-: OrderStdoutWrittenFat("2")):: Nil))
      assert(runningSince.elapsed >= 100.millis)
    }

    "/fatEvent?after=202, with delay=0" in {
      val runningSince = now
      scheduler.scheduleOnce(100.millis) {
        eventWatch.addStamped(Stamped(EventId(203), OrderId("10") <-: OrderStdoutWritten("3")))
      }
      assert(getFatEventSeq("/fatEvent?delay=0&timeout=30&after=202") == EventSeq.NonEmpty(
        Stamped(203, OrderId("10") <-: OrderStdoutWrittenFat("3")):: Nil))
      assert(runningSince.elapsed >= EventDirectives.MinimumDelay)
    }

    "/fatEvent?after=203 no more events, with timeout" in {
      val runningSince = now
      assert(getFatEventSeq("/fatEvent?after=203&timeout=0.1") == EventSeq.Empty(203))
      assert(runningSince.elapsed >= 100.millis)
    }

    "After truncated journal" in {
      eventWatch.tear(50)
      assert(getFatEventSeq("/fatEvent?after=49") == TearableEventSeq.Torn(50))
      assert(getFatEventSeq("/fatEvent?limit=3&after=50") == TearableEventSeq.Torn(50))  // Must rewind to torn EventId.FirstEventId
    }
  }

  "TooManyRequests on concurrent request" in {
    Get("/fatEvent?timeout=9&after=203") ~> Accept(`application/json`) ~> route ~> check {
      assert(status != TooManyRequests, status.toString)
    }
  }

  private def getFatEvents(uri: String): Seq[Stamped[KeyedEvent[FatEvent]]] =
    getFatEventSeq(uri) match {
      case EventSeq.NonEmpty(stampeds) =>
        assert(stampeds.nonEmpty)
        stampeds

      case x => fail(s"Unexpected response: $x")
    }

  @tailrec
  private def getFatEventSeq(uri: String): TearableEventSeq[Seq, KeyedEvent[FatEvent]] = {
    var retryCount = 0
    Get(uri) ~> Accept(`application/json`) ~> route ~> check {
      status match {
        case TooManyRequests if retryCount < 1000 =>
          retryCount += 1
          logger.debug("TooManyRequests #" + retryCount)
          sleep(10.ms)
          None
        case OK => Some(responseAs[TearableEventSeq[Seq, KeyedEvent[FatEvent]]])
        case _ => fail(s"$status - ${responseEntity.toStrict(timeout).value}")
      }
    } match {
      case None => getFatEventSeq(uri)
      case Some(result) => result
    }
  }

  protected final class TestEventWatch extends EventCollector(EventCollector.Configuration.ForTest) {
    var lastEventsAfter = EventId(-1)

    // Return a minimum snapshot
    def snapshotObjectsFor(after: EventId) = Some(EventId.BeforeFirst ->
      CloseableIterator(
        JournalHeader.initial(JournalId(randomUUID())),  // JournalHeader is implicitly a snapshot object
        masterMetaState))

    override def eventsAfter(after: EventId) = {
      lastEventsAfter = after
      super.eventsAfter(after)
    }
  }
}

object FatEventRouteTest
{
  private val logger = Logger(getClass)
  private val sign = new FileBasedSigner(new SillySigner, MasterFileBaseds.jsonCodec).sign _
  private val TestVersionId = VersionId("VERSION")
  private val TestAgentRefId = AgentRefPath("/AGENT") ~ TestVersionId
  private val TestWorkflow = Workflow.of(
    WorkflowPath("/test") ~ TestVersionId,
    Execute(WorkflowJob(TestAgentRefId.path, ExecutablePath("/executable"))))
  private val masterMetaState = MasterMetaState(MasterId("FatEventRouteTest"), Timestamp.now)
  private val InitialEvents =
    //Stamped(EventId(1), NoKey <-: MasterReady("UTC", 0.s)) ::  // Not required
    Stamped(EventId(2), NoKey <-: VersionAdded(TestVersionId)) ::
    Stamped(EventId(3), NoKey <-: FileBasedAdded(TestAgentRefId.path, sign(AgentRef(TestAgentRefId, Uri("http://127.0.0.1:0"))))) ::
    Stamped(EventId(4), NoKey <-: FileBasedAdded(TestWorkflow.path, sign(TestWorkflow))) :: Nil

  private val TestEvents: Seq[Seq[Stamped[AnyKeyedEvent]]] =
    (1 to 18).map(i =>
      Stamped(EventId(i * 10    ), OrderId(i.toString) <-: OrderAdded(TestWorkflow.id, None, Map.empty)) ::     // Yields OrderFatEvent
      Stamped(EventId(i * 10 + 1), OrderId(i.toString) <-: OrderAttachable(TestAgentRefId.path)) ::     // No FatEvent
      Stamped(EventId(i * 10 + 2), OrderId(i.toString) <-: OrderTransferredToAgent(TestAgentRefId.path)) ::  // No FatEvent
      Nil)
  private val TestFatEvents: Seq[Stamped[KeyedEvent[OrderAddedFat]]] =
    TestEvents.flatMap(
      _ collectFirst {
        case Stamped(eventId, timestamp, KeyedEvent(orderId: OrderId, _: OrderAdded)) =>
          Stamped(eventId, timestamp, orderId <-: OrderAddedFat(TestWorkflow.id, None, Map.empty))
      })

  private def fatEventsAfter(after: EventId) = TestFatEvents dropWhile (_.eventId <= after)
}
