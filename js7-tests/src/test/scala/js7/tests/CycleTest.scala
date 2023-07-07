package js7.tests

import java.time.{LocalDateTime, LocalTime, ZoneId}
import java.util.concurrent.TimeoutException
import js7.agent.RunningAgent
import js7.base.configutils.Configs.*
import js7.base.log.Logger
import js7.base.problem.Problem
import js7.base.test.OurTestSuite
import js7.base.thread.MonixBlocking.syntax.RichTask
import js7.base.time.JavaTimestamp.local
import js7.base.time.JavaTimestamp.specific.RichJavaTimestamp
import js7.base.time.ScalaTime.DurationRichInt
import js7.base.time.{AdmissionTimeScheme, AlwaysPeriod, DailyPeriod, TestAlarmClock, Timestamp, Timezone}
import js7.base.utils.ScalaUtils.syntax.RichEither
import js7.controller.RunningController
import js7.data.agent.AgentPath
import js7.data.calendar.{Calendar, CalendarPath}
import js7.data.controller.ControllerCommand.{CancelOrders, ResumeOrder}
import js7.data.event.{KeyedEvent, Stamped}
import js7.data.execution.workflow.instructions.ScheduleTester
import js7.data.item.ItemOperation.{AddOrChangeSigned, AddVersion}
import js7.data.item.VersionId
import js7.data.lock.{Lock, LockPath}
import js7.data.order.OrderEvent.{LockDemand, OrderAdded, OrderAttachable, OrderAttached, OrderBroken, OrderCancelled, OrderCaught, OrderCycleFinished, OrderCycleStarted, OrderCyclingPrepared, OrderDeleted, OrderDetachable, OrderDetached, OrderFailed, OrderFinished, OrderLocksAcquired, OrderLocksReleased, OrderMoved, OrderOutcomeAdded, OrderProcessed, OrderProcessingStarted, OrderResumed, OrderStarted, OrderStopped, OrderTerminated}
import js7.data.order.OrderObstacle.WaitingForOtherTime
import js7.data.order.{CycleState, FreshOrder, OrderEvent, OrderId, OrderObstacle, Outcome}
import js7.data.value.expression.ExpressionParser.expr
import js7.data.workflow.instructions.Schedule.{Continuous, Periodic, Scheme, Ticking}
import js7.data.workflow.instructions.{Break, Cycle, Fail, Fork, If, LockInstruction, Options, Schedule, Stop, TryInstruction}
import js7.data.workflow.position.{BranchId, Position}
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.tests.CycleTest.*
import js7.tests.jobs.{EmptyJob, SemaphoreJob}
import js7.tests.testenv.DirectoryProvider.toLocalSubagentId
import js7.tests.testenv.{BlockingItemUpdater, ControllerAgentForScalaTest}
import monix.eval.Task
import monix.execution.Scheduler.Implicits.traced
import monix.reactive.Observable
import scala.collection.immutable.VectorBuilder
import scala.concurrent.duration.*

final class CycleTest extends OurTestSuite
with ControllerAgentForScalaTest with ScheduleTester with BlockingItemUpdater
{
  protected val agentPaths = Seq(agentPath)
  protected val items = Seq(calendar, cycleTestExampleCalendar, cycleTestExampleWorkflow, lock)

  override protected def controllerConfig = config"""
    js7.auth.users.TEST-USER.permissions = [ UpdateItem ]
    js7.controller.agent-driver.command-batch-delay = 0ms
    js7.controller.agent-driver.event-buffer-delay = 0ms
    """
  override protected def agentConfig = config"""
    js7.job.execution.signed-script-injection-allowed = on
    """

  private implicit val clock: TestAlarmClock = CycleTest.clock

  override protected def controllerTestWiring = RunningController.TestWiring(
    alarmClock = Some(clock))

  override protected def agentTestWiring = RunningAgent.TestWiring(
    alarmClock = Some(clock))

  private implicit val zone: ZoneId = CycleTest.zone

  "Cycle with empty Schedule" in {
    val workflow = updateItem(Workflow(
      WorkflowPath("EMPTY"),
      Seq(
        Cycle(
          Schedule(Nil),
          Workflow.of(Fail(Some(expr("'TEST FAILURE'")))))),
      timeZone = timezone,
      calendarPath = Some(calendar.path)))

    val events = controller.runOrder(FreshOrder(OrderId("#2021-10-01#EMPTY"), workflow.path))
      .map(_.value)
    assert(events == Seq(
      OrderAdded(workflow.id),
      OrderStarted,
      OrderMoved(Position(1)),
      OrderFinished()))
  }

  "Simple loop" in {
    val workflow = updateItem(Workflow(
      WorkflowPath("SIMPLE-LOOP"),
      Seq(
        EmptyJob.execute(agentPath),  // Let start Cycle at Agent
        Cycle(
          Schedule(Seq(
            Scheme(
              AdmissionTimeScheme(Seq(AlwaysPeriod)),
              Continuous(pause = 0.s, limit = Some(3))))),
          cycleWorkflow = Workflow.of(
            EmptyJob.execute(agentPath)))),
      timeZone = timezone,
      calendarPath = Some(calendar.path)))

    val events = controller.runOrder(FreshOrder(OrderId("#2021-10-01#SIMPLE-LOOP"), workflow.path))
      .map(_.value)
    val until = local("2021-10-02T00:00")
    val cycleState =
      CycleState(end = until, index = 1, next = Timestamp.Epoch)
    assert(events == Seq(
      OrderAdded(workflow.id),
      OrderAttachable(agentPath),
      OrderAttached(agentPath),
      OrderStarted,
      OrderProcessingStarted(subagentId),
      OrderProcessed(Outcome.succeeded),
      OrderMoved(Position(1)),

      OrderCyclingPrepared(cycleState),

      OrderCycleStarted,
      OrderProcessingStarted(subagentId),
      OrderProcessed(Outcome.succeeded),
      OrderMoved(Position(1) / "cycle+end=1633122000000,i=1" % 1),
      OrderCycleFinished(Some(cycleState.copy(index = 2))),

      OrderCycleStarted,
      OrderProcessingStarted(subagentId),
      OrderProcessed(Outcome.succeeded),
      OrderMoved(Position(1) / "cycle+end=1633122000000,i=2" % 1),
      OrderCycleFinished(Some(cycleState.copy(index = 3))),

      OrderCycleStarted,
      OrderProcessingStarted(subagentId),
      OrderProcessed(Outcome.succeeded),
      OrderMoved(Position(1) / "cycle+end=1633122000000,i=3" % 1),
      OrderCycleFinished(None),
      OrderMoved(Position(2)),

      OrderDetachable,
      OrderDetached,
      OrderFinished()))
  }

  "Continuously(pause = 0, limit = high) with empty body is detected and fails" in {
    pending // FIXME
    // Wenn bei pause=0 zwischen OrderCycleStarted und OrderCycleFinished kein relevantes Event
    // aufgetreten ist, soll die Schleife abgebrochen werden.
    // Denn JS7 berechnet die Events alle im Voraus und platzt dabei.
    val workflow = updateItem(Workflow(
      WorkflowPath("ENDLESS"),
      Seq(
        Cycle(
          Schedule(Seq(Scheme(
            AdmissionTimeScheme(Seq(AlwaysPeriod)),
            Continuous(pause = 0.s, limit = Some(Int.MaxValue))))),
          Workflow.empty)),
      timeZone = timezone,
      calendarPath = Some(calendar.path)))
    val events = controller.runOrder(FreshOrder(OrderId("ENDLESS"), workflow.path))
      .map(_.value)
  }

  "Failing cycle" in {
    val workflow = updateItem(Workflow(
      WorkflowPath("FAILING"),
      Seq(
        Cycle(
          Schedule(Seq(Scheme(
            AdmissionTimeScheme(Seq(AlwaysPeriod)),
            Continuous(pause = 0.s, limit = Some(1))))),
          Workflow.of(Fail(Some(expr("'TEST FAILURE'")))))),
      timeZone = timezone,
      calendarPath = Some(calendar.path)))

    val orderDate = "2021-10-01"
    val events = controller.runOrder(FreshOrder(OrderId(s"#$orderDate#FAILING"), workflow.path))
      .map(_.value)
    val cycleState = CycleState(
      end = local("2021-10-02T00:00"),
      index = 1,
      next = Timestamp.Epoch)
    assert(events == Seq(
      OrderAdded(workflow.id),
      OrderStarted,
      OrderCyclingPrepared(cycleState),
      OrderCycleStarted,
      OrderOutcomeAdded(Outcome.Failed(Some("TEST FAILURE"))),
      OrderFailed(Position(0) / BranchId.cycle(cycleState) % 0)))
  }

  "Catching a failing cycle" in {
    val workflow = updateItem(Workflow(
      WorkflowPath("CATCH"),
      Seq(
        TryInstruction(
          Workflow.of(
            Cycle(
              Schedule(Seq(Scheme(
                AdmissionTimeScheme(Seq(AlwaysPeriod)),
                Continuous(pause = 0.s, limit = Some(1))))),
              Workflow.of(Fail(Some(expr("'TEST FAILURE'")))))),
          Workflow.empty)),
      timeZone = timezone,
      calendarPath = Some(calendar.path)))

    val orderDate = "2021-10-01"
    val events = controller.runOrder(FreshOrder(OrderId(s"#$orderDate#CATCH"), workflow.path))
      .map(_.value)
    val cycleState = CycleState(
      end = local("2021-10-02T00:00"),
      index = 1,
      next = Timestamp.Epoch)
    assert(events == Seq(
      OrderAdded(workflow.id),
      OrderMoved(Position(0) / "try+0" % 0),
      OrderStarted,
      OrderCyclingPrepared(cycleState),
      OrderCycleStarted,
      OrderOutcomeAdded(Outcome.Failed(Some("TEST FAILURE"))),
      OrderCaught(Position(0) / "catch+0" % 0),
      OrderMoved(Position(1)),
      OrderFinished()))
  }

  "Cancel while in Order.BetweenCycle" in {
    clock.resetTo(local("2021-10-01T00:00"))
    val orderDate = "2021-10-01"
    val workflow = updateItem(Workflow(
      WorkflowPath("CANCEL"),
      Seq(
        Cycle(
          Schedule(Seq(Scheme(
            AdmissionTimeScheme(Seq(
              DailyPeriod(LocalTime.parse("18:00"), 1.s))),
            Periodic(1.h, Seq(0.s))))),
          Workflow.of(Fail(Some(expr("'TEST FAILURE'")))))),
      timeZone = timezone,
      calendarPath = Some(calendar.path)))

    val eventId = eventWatch.lastAddedEventId
    val orderId = OrderId(s"#$orderDate#CANCEL")
    controller.api.addOrder(FreshOrder(orderId, workflow.path)).await(99.s).orThrow
    eventWatch.await[OrderCyclingPrepared](_.key == orderId, after = eventId)

    controller.api.executeCommand(CancelOrders(Seq(orderId))).await(99.s).orThrow
    eventWatch.await[OrderCancelled](_.key == orderId, after = eventId)
      .map(_.value.event)

    assert(eventWatch.eventsByKey[OrderEvent](key = orderId, after = eventId) == Seq(
      OrderAdded(workflow.id),
      OrderStarted,
      OrderCyclingPrepared(CycleState(
        end = local("2021-10-02T00:00"),
        index = 1,
        next = local("2021-10-01T18:00"))),
      OrderCancelled))
  }

/*
  "Cancel the running cycle only" in {
    New command to cancel only within the block ?
    and then continue with next cycle.
    CancelOrder(label="..")
    Oder besonderes Kommando zum Zyklus abzubrechen,
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

    val workflow = updateItem(Workflow(
      WorkflowPath("DST"),
      Seq(
        Cycle(
          Schedule(Seq(Scheme(
            AdmissionTimeScheme(Seq(
              AlwaysPeriod)),
            Periodic(1.h, Seq(0.minute, 30.minute))))),
          Workflow.empty)),
      timeZone = timezone,
      calendarPath = Some(calendar.path)))

    def orderCycleStartedTimetamps(orderId: OrderId) =
      eventWatch
        .allStamped[OrderCycleStarted]
        .collect {
          case stamped @ Stamped(_, _, KeyedEvent(`orderId`, OrderCycleStarted)) =>
            stamped.timestamp
        }

    "Winter to summer" in {
      clock.resetTo(Timestamp("2021-03-28T01:00:00Z"))
      var eventId = eventWatch.lastAddedEventId
      val orderId = OrderId("#2021-03-28#SUMMER")
      controller.api.addOrder(FreshOrder(orderId, workflow.path))
        .await(99.s).orThrow
      eventWatch.await[OrderCyclingPrepared](_.key == orderId, after = eventId)
      assert(orderToObstacles(orderId) ==
        Right(Set[OrderObstacle](WaitingForOtherTime(local("2021-03-28T03:30")))))

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

      controller.api.executeCommand(CancelOrders(Seq(orderId))).await(99.s).orThrow
    }

    "Summer to winter" in {
      clock.resetTo(Timestamp("2021-10-31T00:00:00Z"))
      var eventId = eventWatch.lastAddedEventId
      val orderId = OrderId("#2021-10-31#WINTER")
      controller.api.addOrder(FreshOrder(orderId, workflow.path))
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

      controller.api.executeCommand(CancelOrders(Seq(orderId))).await(99.s).orThrow
    }
  }

  "SchedulerTester standard example" - {
    // Test the js7-data CycleTest JSON example.
    clock.resetTo(local("2021-10-01T00:00"))

    addStandardScheduleTests { (timeInterval, cycleDuration, zone, expected) =>
      var eventId = eventWatch.lastAddedEventId
      clock.resetTo(timeInterval.start - 1.s)  // Start the order early

      val orderDate = timeInterval.start.toLocalDateTime(zone).toLocalDate
      val orderId = OrderId(s"#$orderDate#CycleTesterTest")
      logger.debug(s"addOrder $orderId")
      controller.api
        .addOrder(FreshOrder(orderId, cycleTestExampleWorkflow.path, deleteWhenTerminated = true))
        .await(99.s).orThrow

      eventWatch.await[OrderCyclingPrepared](_.key == orderId)
      val cycleStartedTimes = new VectorBuilder[Timestamp]
      val expectedCycleStartTimes = expected
        .map { case (cycleWaitTimestamp, cycleState) =>
          cycleWaitTimestamp max cycleState.next // Expected time of OrderCycleStart
        }

      for (t <- expectedCycleStartTimes) {
        clock := t  // Difference may be zero, so OrderCycleStarted may already have been emitted
        val stamped = eventWatch
          .await[OrderCycleStarted](_.key == orderId, after = eventId)
          .head
        cycleStartedTimes += stamped.timestamp
        eventId = stamped.eventId

        clock += cycleDuration
        TestJob.continue()
        eventId = eventWatch.await[OrderCycleFinished](_.key == orderId, after = eventId)
          .head.eventId
      }
      assert(cycleStartedTimes.result() == expectedCycleStartTimes)

      for (ts <- cycleStartedTimes.result().lastOption) {
        clock := ts + cycleDuration
      }
      eventWatch.await[OrderFinished](_.key == orderId, after = eventId)
    }
  }

  "One first cycle in mid of period (bug JS-2012)" in {
    // Fixed bug:
    // Cycle executes the block twice, when starting after the first period of the calendar day.
    clock.resetTo(local("2021-10-01T01:30"))
    val workflow = updateItem(Workflow(
      WorkflowPath("ONCE-AN-HOUR"),
      Seq(
        Cycle(
          Schedule(Seq(Scheme(
            AdmissionTimeScheme(Seq(AlwaysPeriod)),
            Ticking(1.h)))),
          Workflow.empty)),
      timeZone = timezone,
      calendarPath = Some(calendar.path)))

    var eventId = eventWatch.lastAddedEventId
    val orderId = OrderId("#2021-10-01#ONCE-A-DAY")
    controller.api.addOrder(FreshOrder(orderId, workflow.path))
      .await(99.s).orThrow

    clock.tick()
    eventId = eventWatch.await[OrderCycleStarted](_.key == orderId, after = eventId).head.eventId
    assert(eventWatch.eventsByKey[OrderEvent](orderId).count(_ == OrderCycleStarted) == 1)

    clock.tick(30.minutes - 1.s)
    assert(eventWatch.eventsByKey[OrderEvent](orderId).count(_ == OrderCycleStarted) == 1)

    clock.tick(1.s)
    eventId = eventWatch.await[OrderCycleStarted](_.key == orderId, after = eventId).head.eventId
    assert(eventWatch.eventsByKey[OrderEvent](orderId).count(_ == OrderCycleStarted) == 2)

    clock.tick(1.h - 1.s)
    assert(eventWatch.eventsByKey[OrderEvent](orderId).count(_ == OrderCycleStarted) == 2)

    clock.tick(1.s)
    eventId = eventWatch.await[OrderCycleStarted](_.key == orderId, after = eventId).head.eventId
    assert(eventWatch.eventsByKey[OrderEvent](orderId).count(_ == OrderCycleStarted) == 3)
  }

  "Break" - {
    "Break" in {
      clock.resetTo(local("2023-03-21T00:00"))

      val workflow = Workflow(
        WorkflowPath("BREAK-WORKFLOW"),
        timeZone = timezone,
        calendarPath = Some(calendar.path),
        instructions = Seq(
          Cycle(
            Schedule(Seq(Scheme(
              AdmissionTimeScheme(Seq(AlwaysPeriod)),
              Continuous(pause = 0.s, limit = Some(1))))),
            Workflow.of(
              If(
                expr("true"),
                Workflow.of(
                  LockInstruction(
                    List(LockDemand(lock.path)),
                    Workflow.of(
                      TryInstruction(
                        Workflow.of(
                          EmptyJob.execute(agentPath),
                          If(expr("true"), Workflow.of(
                            Break())),
                          Fail()),
                        Workflow.of(
                          Fail()))))))))))

      withTemporaryItem(workflow) { workflow =>
        val orderId = OrderId("#2023-03-21#BREAK")
        controller.api.addOrder(FreshOrder(orderId, workflow.path, deleteWhenTerminated = true))
          .await(99.s).orThrow
        eventWatch.await[OrderTerminated](_.key == orderId)
        eventWatch.await[OrderDeleted](_.key == orderId)

        assert(eventWatch.eventsByKey[OrderEvent](orderId) == Seq(
          OrderAdded(workflow.id, deleteWhenTerminated = true),
          OrderStarted,

          OrderCyclingPrepared(
            CycleState(end = local("2023-03-22T00:00"), 0, 0, 1, next = Timestamp.Epoch)),
          OrderCycleStarted,

          OrderMoved(Position(0) / "cycle+end=1679436000000,i=1" % 0 / "then" % 0),
          OrderLocksAcquired(List(LockDemand(lock.path))),
          OrderMoved(Position(0) / "cycle+end=1679436000000,i=1" % 0 / "then" % 0 / "lock" % 0 / "try+0" % 0),
          OrderAttachable(agentPath),
          OrderAttached(agentPath),
          OrderProcessingStarted(Some(subagentId)),
          OrderProcessed(Outcome.succeeded),
          OrderMoved(Position(0) / "cycle+end=1679436000000,i=1" % 0 / "then" % 0 / "lock" % 0 / "try+0" % 1 / "then" % 0),
          OrderDetachable,
          OrderDetached,
          OrderLocksReleased(List(lock.path)),
          OrderMoved(Position(0) / "cycle+end=1679436000000,i=1" % 1),

          OrderCycleFinished(None),

          OrderMoved(Position(1)),
          OrderFinished(None),
          OrderDeleted))
      }
    }

    "Break in Options" in {
      clock.resetTo(local("2023-03-21T00:00"))

      val workflow = Workflow(
        WorkflowPath("OPTIONS-BREAK-WORKFLOW"),
        timeZone = timezone,
        calendarPath = Some(calendar.path),
        instructions = Seq(
          Options(stopOnFailure = Some(true),
            block = Workflow.of(
              Cycle(
                Schedule(Seq(Scheme(
                  AdmissionTimeScheme(Seq(AlwaysPeriod)),
                  Continuous(pause = 0.s, limit = Some(1))))),
                Workflow.of(
                  EmptyJob.execute(agentPath),
                  If(expr("true"), Workflow.of(
                    Break())),
                  Fail()))))))

      withTemporaryItem(workflow) { workflow =>
        val orderId = OrderId("#2023-03-21#OPTIONS-BREAK")
        controller.api.addOrder(FreshOrder(orderId, workflow.path, deleteWhenTerminated = true))
          .await(99.s).orThrow
        eventWatch.await[OrderTerminated](_.key == orderId)
        eventWatch.await[OrderDeleted](_.key == orderId)

        assert(eventWatch.eventsByKey[OrderEvent](orderId) == Seq(
          OrderAdded(workflow.id, deleteWhenTerminated = true),
          OrderMoved(Position(0) / "options" % 0),
          OrderStarted,

          OrderCyclingPrepared(
            CycleState(end = local("2023-03-22T00:00"), 0, 0, 1, next = Timestamp.Epoch)),
          OrderCycleStarted,

          OrderAttachable(agentPath),
          OrderAttached(agentPath),
          OrderProcessingStarted(Some(subagentId)),
          OrderProcessed(Outcome.succeeded),
          OrderMoved(Position(0) / "options" % 0 / "cycle+end=1679436000000,i=1" % 1 / "then" % 0),
          OrderMoved(Position(0) / "options" % 0 / "cycle+end=1679436000000,i=1" % 1),

          OrderCycleFinished(None),

          OrderMoved(Position(1)),
          OrderDetachable,
          OrderDetached,
          OrderFinished(None),
          OrderDeleted))
      }
    }

    "Break without Cycle is rejected" in {
      val versionId = VersionId("WILL-BE-REJECTED")
      val checked = controller.api
        .updateItems(Observable(
          AddVersion(versionId),
          AddOrChangeSigned(toSignedString(Workflow.of(
            WorkflowPath("WILL-BE-REJECTED") ~ versionId,
            If(expr("true"), Workflow.of(
              Break())))))))
        .await(99.s)

      assert(checked == Left(Problem(
        "JSON DecodingFailure at : Break instruction at 0/then:0 without Cycle")))
    }

    "Break in Fork without Cycle is rejected" in {
      val versionId = VersionId("WILL-BE-REJECTED")
      val checked = controller.api
        .updateItems(Observable(
          AddVersion(versionId),
          AddOrChangeSigned(toSignedString(Workflow(
            WorkflowPath("WILL-BE-REJECTED") ~ versionId,
            timeZone = timezone,
            calendarPath = Some(calendar.path),
            instructions = Seq(Cycle(Schedule(Nil),
              Workflow.of(
                Fork.of(
                  "A" -> Workflow.of(
                    If(expr("true"), Workflow.of(
                      Break()))))))))))))
        .await(99.s)

      assert(checked == Left(Problem(
        "JSON DecodingFailure at : Break instruction at 0/cycle:0/fork+A:0/then:0 without Cycle")))
    }
  }

  "Resume with invalid Cycle BranchId lets the Order fail" in {
    clock.resetTo(local("2023-03-21T00:00"))
    val workflow = Workflow(
      WorkflowPath("BROKEN-WORKFLOW"),
      timeZone = timezone,
      calendarPath = Some(calendar.path),
      instructions = Seq(
        Cycle(
          Schedule(Seq(Scheme(
            AdmissionTimeScheme(Seq(AlwaysPeriod)),
            Continuous(1.s)))),
          Workflow.of(
            EmptyJob.execute(agentPath),
            Stop()))))
    withTemporaryItem(workflow) { workflow =>
      val orderId = OrderId("#2023-03-21#BROKEN")
      controller.api.addOrder(FreshOrder(orderId, workflow.path, deleteWhenTerminated = true))
        .await(99.s).orThrow
      eventWatch.await[OrderStopped](_.key == orderId)

      controller.api
        .executeCommand(
          ResumeOrder(orderId, position = Some(Position(0) / "cycle+💣" % 2)))
        .await(99.s).orThrow
      eventWatch.await[OrderBroken](_.key == orderId)

      controller.api.executeCommand(CancelOrders(Seq(orderId))).await(99.s).orThrow
      eventWatch.await[OrderTerminated](_.key == orderId)
      eventWatch.await[OrderDeleted](_.key == orderId)

      assert(eventWatch.eventsByKey[OrderEvent](orderId)
        .filter {
          case _: OrderCyclingPrepared => false
          case _: OrderMoved => false
          case _ => true
        } ==
        Seq(
          OrderAdded(workflow.id, deleteWhenTerminated = true),
          OrderStarted,
          OrderCycleStarted,
          OrderAttachable(agentPath),
          OrderAttached(agentPath),
          OrderProcessingStarted(Some(subagentId)),
          OrderProcessed(Outcome.succeeded),
          OrderDetachable,
          OrderDetached,
          OrderStopped,
          OrderResumed(Some(Position(0) / "cycle+💣" % 2)),
          OrderOutcomeAdded(Outcome.Disrupted(Problem("Expected a Cycle BranchId but got: cycle+💣"))),
          OrderFailed(Position(0) / "cycle+💣" % 2),
          OrderOutcomeAdded(Outcome.Disrupted(Problem("Expected a Cycle BranchId but got: cycle+💣"))),
          OrderBroken(None),
          OrderCancelled,
          OrderDeleted))
    }
  }
}

object CycleTest
{
  private val logger = Logger[this.type]

  private val agentPath = AgentPath("AGENT")
  private val subagentId = toLocalSubagentId(agentPath)
  private implicit val zone: ZoneId = ZoneId.of("Europe/Mariehamn")
  private val timezone = Timezone(zone.getId)
  private val clock = TestAlarmClock(local("2021-10-01T04:00"))

  private val calendar = Calendar.jocStandard(CalendarPath("CALENDAR"), dateOffset = 0.h)

  private val cycleTestExampleCalendar = Calendar.jocStandard(
    CalendarPath("CycleTest-example"),
    dateOffset = ScheduleTester.dateOffset)

  private val cycleTestExampleWorkflow =
    Workflow(WorkflowPath("CycleTest-example"),
      Seq(
        js7.data.workflow.instructions.CycleTest.exampleCycle
          .copy(
            cycleWorkflow = Workflow.of(
              TestJob.execute(agentPath)))),
      timeZone = timezone,
      calendarPath = Some(cycleTestExampleCalendar.path))

  private class TestJob extends SemaphoreJob(TestJob) {
    override def onAcquired(step: Step, semaphoreName: String) =
      Task {
        val now = clock.now()
        logger.info(s"🔹 $now  ${LocalDateTime.ofInstant(now.toInstant, zone)}")
        Outcome.succeeded
      }
  }
  private object TestJob extends SemaphoreJob.Companion[TestJob]

  private val lock = Lock(LockPath("LOCK"))

  // Use this Log4j Clock with the properties
  // -Dlog4j2.Clock=js7.tests.CycleTest$CycleTestLog4jClock -Duser.timezone=Europe/Mariehamn
  final class CycleTestLog4jClock extends org.apache.logging.log4j.core.util.Clock
  {
    def currentTimeMillis() = clock.epochMilli()
  }
}
