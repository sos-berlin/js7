package js7.tests

import com.google.inject.{AbstractModule, Provides}
import java.time.{LocalTime, ZoneId}
import java.util.concurrent.TimeoutException
import javax.inject.Singleton
import js7.base.configutils.Configs._
import js7.base.thread.MonixBlocking.syntax.RichTask
import js7.base.time.JavaTimestamp.local
import js7.base.time.ScalaTime.DurationRichInt
import js7.base.time.{AdmissionTimeScheme, AlarmClock, AlwaysPeriod, DailyPeriod, TestAlarmClock, Timestamp, Timezone}
import js7.base.utils.ScalaUtils.syntax.RichEither
import js7.data.agent.AgentPath
import js7.data.calendar.{Calendar, CalendarPath}
import js7.data.controller.ControllerCommand.CancelOrders
import js7.data.event.{EventSeq, KeyedEvent, Stamped}
import js7.data.execution.workflow.instructions.CycleTester
import js7.data.item.VersionId
import js7.data.order.OrderEvent.{OrderAdded, OrderAttachable, OrderAttached, OrderCancelled, OrderCatched, OrderCycleFinished, OrderCycleStarted, OrderCyclingPrepared, OrderDetachable, OrderDetached, OrderFailed, OrderFinished, OrderMoved, OrderProcessed, OrderProcessingStarted, OrderStarted}
import js7.data.order.{CycleState, FreshOrder, OrderEvent, OrderId, Outcome}
import js7.data.value.expression.ExpressionParser.expr
import js7.data.workflow.instructions.Schedule.{Periodic, Scheme}
import js7.data.workflow.instructions.{Cycle, Fail, Schedule, TryInstruction}
import js7.data.workflow.position.{BranchId, Position}
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.tests.CycleTest._
import js7.tests.jobs.EmptyJob
import js7.tests.testenv.ControllerAgentForScalaTest
import monix.execution.Scheduler.Implicits.global
import org.scalatest.freespec.AnyFreeSpec
import scala.concurrent.duration._

final class CycleTest extends AnyFreeSpec with ControllerAgentForScalaTest with CycleTester
{
  protected val agentPaths = Seq(agentPath)
  protected val items = Seq(calendar)

  override protected def controllerConfig = config"""
    js7.auth.users.TEST-USER.permissions = [ UpdateItem ]
    js7.controller.agent-driver.command-batch-delay = 0ms
    js7.controller.agent-driver.event-buffer-delay = 0ms
    """
  override protected def agentConfig = config"""
    js7.job.execution.signed-script-injection-allowed = on
    """

  override protected def controllerModule = new AbstractModule {
    @Provides @Singleton def provideAlarmClock(): AlarmClock = clock
  }
  override protected def agentModule = new AbstractModule {
    @Provides @Singleton def provideAlarmClock(): AlarmClock = clock
  }

  private implicit val zone = CycleTest.zone
  private lazy val versionIdIterator = Iterator.from(1).map(i => VersionId(i.toString))

  "Cycle with empty Schedule" in {
    val workflow = addWorkflow(Workflow(
      WorkflowPath("EMPTY"),
      Seq(
        Cycle(
          Schedule(Nil),
          Workflow.of(Fail(Some(expr("'TEST FAILURE'")))))),
      calendarPath = Some(calendar.path)))

    val events = controller.runOrder(FreshOrder(OrderId("#2021-10-01#EMPTY"), workflow.path))
      .map(_.value)
    assert(events == Seq(
      OrderAdded(workflow.id),
      OrderStarted,
      OrderMoved(Position(1)),
      OrderFinished))
  }

  "Simple loop" in {
    val workflow = addWorkflow(Workflow(
      WorkflowPath("SIMPLE-LOOP"),
      Seq(
        Cycle(
          Schedule(Seq(
            Scheme(
              AdmissionTimeScheme(Seq(AlwaysPeriod)),
              Schedule.Continuous(pause = 0.s, limit = Some(3))))),
          cycleWorkflow = Workflow.of(
            EmptyJob.execute(agentPath)))),
      calendarPath = Some(calendar.path)))

    val events = controller.runOrder(FreshOrder(OrderId("#2021-10-01#SIMPLE-LOOP"), workflow.path))
      .map(_.value)
    val until = local("2021-10-02T00:00")
    val cycleState =
      CycleState(end = until, schemeIndex = 0, index = 1, next = Timestamp.Epoch)
    assert(events == Seq(
      OrderAdded(workflow.id),
      OrderStarted,
      OrderCyclingPrepared(cycleState),

      OrderCycleStarted,
      OrderAttachable(agentPath),
      OrderAttached(agentPath),
      OrderProcessingStarted,
      OrderProcessed(Outcome.succeeded),
      OrderMoved(Position(0) / "cycle+end=1633122000000,i=1" % 1),
      OrderCycleFinished(Some(cycleState.copy(index = 2))),

      OrderCycleStarted,
      OrderProcessingStarted,
      OrderProcessed(Outcome.succeeded),
      OrderMoved(Position(0) / "cycle+end=1633122000000,i=2" % 1),
      OrderCycleFinished(Some(cycleState.copy(index = 3))),

      OrderCycleStarted,
      OrderProcessingStarted,
      OrderProcessed(Outcome.succeeded),
      OrderMoved(Position(0) / "cycle+end=1633122000000,i=3" % 1),
      OrderCycleFinished(None),
      OrderMoved(Position(1)),

      OrderDetachable,
      OrderDetached,
      OrderFinished))
  }

  "Continously(pause = 0, limit = high) with empty body is detected and fails" in {
    pending // FIXME
    // Wenn bei pause=0 zwischen OrderCycleStarted und OrderCycleFinished kein relevantes Event
    // aufgetreten ist, soll die Schleife abgebrochen werden.
    // Denn JS7 berechnet die Events alle im Voraus und platzt dabei.
    val workflow = addWorkflow(Workflow(
      WorkflowPath("ENDLESS"),
      Seq(
        Cycle(
          Schedule(Seq(Schedule.Scheme(
            AdmissionTimeScheme(Seq(AlwaysPeriod)),
            Schedule.Continuous(pause = 0.s, limit = Some(Int.MaxValue))))),
          Workflow.empty)),
      calendarPath = Some(calendar.path)))
    val events = controller.runOrder(FreshOrder(OrderId("ENDLESS"), workflow.path))
      .map(_.value)
  }

  "Failing cycle" in {
    val workflow = addWorkflow(Workflow(
      WorkflowPath("FAILING"),
      Seq(
        Cycle(
          Schedule(Seq(Scheme(
            AdmissionTimeScheme(Seq(AlwaysPeriod)),
            Schedule.Continuous(pause = 0.s, limit = Some(1))))),
          Workflow.of(Fail(Some(expr("'TEST FAILURE'")))))),
      calendarPath = Some(calendar.path)))

    val orderDate = "2021-10-01"
    val events = controller.runOrder(FreshOrder(OrderId(s"#$orderDate#FAILING"), workflow.path))
      .map(_.value)
    val cycleState = CycleState(
      end = local("2021-10-02T00:00"),
      schemeIndex = 0,
      index = 1,
      next = Timestamp.Epoch)
    assert(events == Seq(
      OrderAdded(workflow.id),
      OrderStarted,
      OrderCyclingPrepared(cycleState),
      OrderCycleStarted,
      OrderFailed(
        Position(0) / BranchId.cycle(cycleState) % 0,
        Some(Outcome.Failed(Some("TEST FAILURE"))))))
  }

  "Catching a failing cycle" in {
    val workflow = addWorkflow(Workflow(
      WorkflowPath("CATCH"),
      Seq(
        TryInstruction(
          Workflow.of(
            Cycle(
              Schedule(Seq(Scheme(
                AdmissionTimeScheme(Seq(AlwaysPeriod)),
                Schedule.Continuous(pause = 0.s, limit = Some(1))))),
              Workflow.of(Fail(Some(expr("'TEST FAILURE'")))))),
          Workflow.empty)),
      calendarPath = Some(calendar.path)))

    val orderDate = "2021-10-01"
    val events = controller.runOrder(FreshOrder(OrderId(s"#$orderDate#CATCH"), workflow.path))
      .map(_.value)
    val cycleState = CycleState(
      end = local("2021-10-02T00:00"),
      schemeIndex = 0,
      index = 1,
      next = Timestamp.Epoch)
    assert(events == Seq(
      OrderAdded(workflow.id),
      OrderMoved(Position(0) / "try+0" % 0),
      OrderStarted,
      OrderCyclingPrepared(cycleState),
      OrderCycleStarted,
      OrderCatched(
        Position(0) / "catch+0" % 0,
        Some(Outcome.Failed(Some("TEST FAILURE")))),
      OrderMoved(Position(1)),
      OrderFinished))
  }

  "Cancel while in Order.BetweenCycle" in {
    clock.resetTo(local("2021-10-01T00:00"))
    val orderDate = "2021-10-01"
    val workflow = addWorkflow(Workflow(
      WorkflowPath("CANCEL"),
      Seq(
        Cycle(
          Schedule(Seq(Scheme(
            AdmissionTimeScheme(Seq(
              DailyPeriod(LocalTime.parse("18:00"), 1.s))),
            Periodic(1.h, Seq(0.s))))),
          Workflow.of(Fail(Some(expr("'TEST FAILURE'")))))),
      calendarPath = Some(calendar.path)))

    val eventId = eventWatch.lastAddedEventId
    val orderId = OrderId(s"#$orderDate#CANCEL")
    controllerApi.addOrder(FreshOrder(orderId, workflow.path)).await(99.s).orThrow
    eventWatch.await[OrderCyclingPrepared](_.key == orderId, after = eventId)

    controllerApi.executeCommand(CancelOrders(Seq(orderId))).await(99.s).orThrow
    eventWatch.await[OrderCancelled](_.key == orderId, after = eventId)
      .map(_.value.event)

    assert(eventWatch.keyedEvents[OrderEvent](key = orderId, after = eventId) == Seq(
      OrderAdded(workflow.id),
      OrderStarted,
      OrderCyclingPrepared(CycleState(
        end = local("2021-10-02T00:00"),
        schemeIndex = 0,
        index = 1,
        next = local("2021-10-01T18:00"))),
      OrderCancelled))
  }

/*
  "Cancel the running cycle only" in {
    New command to cancel only within the block ?
    and then continue with next cycle.
    CancelOrder(label="..")
    Oder besonderers Kommando zum Zyklus abzubrechen,
    das auch Angaben für den nächsten Zyklus ändern kann.
  }
*/

  "Daylight saving time change" - {
    "Mariehamn daylight saving time (to be sure)" in {
      assert(local("2021-03-28T02:59") == Timestamp("2021-03-28T00:59:00Z"))
      assert(local("2021-03-28T04:00") == Timestamp("2021-03-28T01:00:00Z"))
      assert(local("2021-03-28T04:00") - local("2021-03-28T02:59:59") == 1.s)

      assert(local("2021-10-31T00:00") == Timestamp("2021-10-30T21:00:00Z"))
      assert(local("2021-11-01T00:00") == Timestamp("2021-10-31T22:00:00Z"))

      assert(local("2021-10-31T03:30") == Timestamp("2021-10-31T00:30:00Z"))
      assert(local("2021-10-31T04:00") == Timestamp("2021-10-31T02:00:00Z"))
      assert(local("2021-10-31T04:30") == Timestamp("2021-10-31T02:30:00Z"))

      assert(local("2021-10-31T04:00") - local("2021-10-31T03:00") == 2.h)
      assert(local("2021-10-31T04:00") - local("2021-10-31T03:59:59") == 1.h + 1.s)
    }

    val workflow = addWorkflow(Workflow(
      WorkflowPath("DST"),
      Seq(
        Cycle(
          Schedule(Seq(Scheme(
            AdmissionTimeScheme(Seq(
              AlwaysPeriod)),
            Periodic(1.h, Seq(0.minute, 30.minute))))),
          Workflow.empty)),
      calendarPath = Some(calendar.path)))

    def orderCycleStartedTimetamps(orderId: OrderId) =
      eventWatch.all[OrderCycleStarted]
        .asInstanceOf[EventSeq.NonEmpty[Seq, KeyedEvent[OrderEvent]]]
        .stamped
        .collect {
          case stamped @ Stamped(_, _, KeyedEvent(`orderId`, OrderCycleStarted)) =>
            stamped.timestamp
        }

    "Winter to summer" in {
      clock.resetTo(Timestamp("2021-03-28T01:00:00Z"))
      var eventId = eventWatch.lastAddedEventId
      val orderId = OrderId("#2021-03-28#SUMMER")
      controllerApi.addOrder(FreshOrder(orderId, workflow.path))
        .await(99.s).orThrow
      eventWatch.await[OrderCyclingPrepared](_.key == orderId, after = eventId)

      for (i <- 1 to 4) {
        if (i > 1) {
          eventId = eventWatch.lastAddedEventId
          clock += 30.minutes
        }
        eventWatch.await[OrderCycleStarted](_.key == orderId, after = eventId)
      }

      assert(orderCycleStartedTimetamps(orderId) == Vector(
        local("2021-03-28T03:00"),
        local("2021-03-28T03:30"),
        local("2021-03-28T05:00"),  // There is no 04:00 local time
        local("2021-03-28T05:30")))

      assert(orderCycleStartedTimetamps(orderId) == Vector(
        Timestamp("2021-03-28T01:00:00Z"),
        Timestamp("2021-03-28T01:30:00Z"),
        Timestamp("2021-03-28T02:00:00Z"),
        Timestamp("2021-03-28T02:30:00Z")))

      controllerApi.executeCommand(CancelOrders(Seq(orderId))).await(99.s).orThrow
    }

    "Summer to winter" in {
      clock.resetTo(Timestamp("2021-10-31T00:00:00Z"))
      var eventId = eventWatch.lastAddedEventId
      val orderId = OrderId("#2021-10-31#WINTER")
      controllerApi.addOrder(FreshOrder(orderId, workflow.path))
        .await(99.s).orThrow

      eventWatch.await[OrderCyclingPrepared](_.key == orderId, after = eventId)

      eventId = eventWatch.lastAddedEventId
      clock := Timestamp("2021-10-31T00:30:00Z")
      eventWatch.await[OrderCycleStarted](_.key == orderId, after = eventId)

      eventId = eventWatch.lastAddedEventId
      clock := Timestamp("2021-10-31T01:00:00Z")
      // This hour is skipped!!
      intercept[TimeoutException] {
        eventWatch.await[OrderCycleStarted](_.key == orderId, after = eventId, timeout = 200.ms)
      }

      eventId = eventWatch.lastAddedEventId
      clock := Timestamp("2021-10-31T01:30:00Z")
      intercept[TimeoutException] {
        eventWatch.await[OrderCycleStarted](_.key == orderId, after = eventId, timeout = 200.ms)
      }

      eventId = eventWatch.lastAddedEventId
      clock := Timestamp("2021-10-31T02:00:00Z")
      eventWatch.await[OrderCycleStarted](_.key == orderId, after = eventId)

      assert(orderCycleStartedTimetamps(orderId) == Vector(
        local("2021-10-31T03:00"),
        local("2021-10-31T03:30"),
        local("2021-10-31T04:00")))

      assert(orderCycleStartedTimetamps(orderId) == Vector(
        Timestamp("2021-10-31T00:00:00Z"),
        Timestamp("2021-10-31T00:30:00Z"),
        Timestamp("2021-10-31T02:00:00Z"))) // 01:00 is skipped!!

      controllerApi.executeCommand(CancelOrders(Seq(orderId))).await(99.s).orThrow
    }
  }

  "Run Cycle on Agent" in {
    pending // TODO
  }

  private def addWorkflow(workflow: Workflow): Workflow = {
    val v = versionIdIterator.next()
    val w = workflow.withVersion(v)
    directoryProvider.updateVersionedItems(controller, v, Seq(workflow))
    w
  }
}

object CycleTest
{
  private val agentPath = AgentPath("AGENT")
  private implicit val zone = ZoneId.of("Europe/Mariehamn")
  private val clock = TestAlarmClock(local("2021-10-01T04:00"))

  private val calendar = Calendar(
    CalendarPath("CALENDAR"),
    Timezone("Europe/Mariehamn"),
    dateOffset = 0.h,  // FIXME Test with dateOffset = 6.h
    orderIdToDatePattern = "#([^#]+)#.*",
    periodDatePattern = "yyyy-MM-dd")

  // Use this Log4j Clock with the properties
  // -Dlog4j2.Clock=js7.tests.CycleTestt$CycleTestLog4jClock -Duser.timezone=Europe/Mariehamn
  final class CycleTestLog4jClock extends org.apache.logging.log4j.core.util.Clock
  {
    def currentTimeMillis() = clock.epochMilli()
  }
}
