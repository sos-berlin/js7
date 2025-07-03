package js7.tests

import java.time.DayOfWeek.{MONDAY, SUNDAY}
import java.time.{LocalDateTime, LocalTime, ZoneId}
import js7.agent.RunningAgent
import js7.base.configutils.Configs.HoconStringInterpolator
import js7.base.test.OurTestSuite
import js7.base.thread.CatsBlocking.syntax.*
import js7.base.time.AdmissionTimeSchemeForJavaTime.*
import js7.base.time.JavaTimestamp.local
import js7.base.time.ScalaTime.*
import js7.base.time.{AdmissionTimeScheme, SpecificDatePeriod, TestAlarmClock, Timezone, WeekdayPeriod}
import js7.base.utils.ScalaUtils.syntax.RichEither
import js7.data.agent.AgentPath
import js7.data.controller.ControllerCommand.CancelOrders
import js7.data.job.ShellScriptExecutable
import js7.data.order.Order.Fresh
import js7.data.order.OrderEvent.{OrderAttached, OrderCancelled, OrderFailed, OrderFinished, OrderProcessingStarted, OrderStdoutWritten}
import js7.data.order.OrderObstacle.waitingForAdmmission
import js7.data.order.{FreshOrder, Order, OrderId}
import js7.data.value.expression.Expression.StringConstant
import js7.data.workflow.instructions.executable.WorkflowJob
import js7.data.workflow.instructions.{AddOrder, Execute, Fork}
import js7.data.workflow.position.Position
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.tester.ScalaTestUtils.awaitAndAssert
import js7.tests.JobAdmissionTimeTest.*
import js7.tests.jobs.EmptyJob
import js7.tests.testenv.ControllerAgentForScalaTest
import scala.concurrent.duration.*

final class JobAdmissionTimeTest extends OurTestSuite, ControllerAgentForScalaTest:

  override protected val controllerConfig = config"""
    js7.auth.users.TEST-USER.permissions = [ UpdateItem ]
    js7.journal.remove-obsolete-files = false
    js7.controller.agent-driver.command-batch-delay = 0ms
    js7.controller.agent-driver.event-buffer-delay = 0ms"""

  override protected def agentConfig = config"""
    js7.job.execution.signed-script-injection-allowed = on"""

  protected def agentPaths = Seq(agentPath)
  protected def items = Seq(mondayWorkflow, sundayWorkflow)

  private given zoneId: ZoneId = JobAdmissionTimeTest.zoneId
  private given clock: TestAlarmClock = TestAlarmClock(local("2021-03-20T00:00"))

  override protected def agentTestWiring = RunningAgent.TestWiring(
    alarmClock = Some(clock))

  "Sunday at start of daylight saving time" - {
    "Wait for start of permission period" in:
      val orderId = OrderId("🍊")
      val eventId = eventWatch.lastAddedEventId
      controller.api.addOrder(FreshOrder(orderId, sundayWorkflow.path)).await(99.s).orThrow
      eventWatch.await[OrderAttached](_.key == orderId, after = eventId)
      assert(controllerState.idToOrder(orderId).isState[Fresh])
      assert(controllerState.idToOrder(orderId).position == Position(0))
      assert(orderToObstacles(orderId) ==
        Right(Set(waitingForAdmmission(local("2021-03-21T03:00")))))

      assert(orderToObstacles(orderId) ==
        Right(Set(waitingForAdmmission(local("2021-03-21T03:00")))))
      assert(orderObstacleCalculator.waitingForAdmissionOrderCount(clock.now()) == 1)

      clock := local("2021-03-21T02:59")
      awaitAndAssert(1.s)(controllerState.idToOrder(orderId).isState[Fresh])
      assert(controllerState.idToOrder(orderId).position == Position(0))
      assert(orderToObstacles(orderId) ==
        Right(Set(waitingForAdmmission(local("2021-03-21T03:00")))))

      clock := local("2021-03-21T03:00")
      eventWatch.await[OrderFinished](_.key == orderId, after = eventId)
      assert(orderObstacleCalculator.waitingForAdmissionOrderCount(clock.now()) == 0)

    "Start order while in permission period" in:
      clock := local("2021-03-21T03:59")
      val eventId = eventWatch.lastAddedEventId
      val orderId = OrderId("🔺")
      controller.api.addOrder(FreshOrder(orderId, sundayWorkflow.path)).await(99.s).orThrow
      eventWatch.await[OrderFinished](_.key == orderId, after = eventId)

    "Start order at end of permission period" in:
      clock := local("2021-03-21T04:00")
      val eventId = eventWatch.lastAddedEventId
      val orderId = OrderId("🔸")
      controller.api.addOrder(FreshOrder(orderId, sundayWorkflow.path)).await(99.s).orThrow
      assert(controllerState.idToOrder(orderId).isState[Fresh])
      assert(controllerState.idToOrder(orderId).position == Position(0))
      assert(orderToObstacles(orderId) ==
        Right(Set(waitingForAdmmission(local("2021-03-28T03:00")))))
      execCmd:
        CancelOrders(orderId :: Nil)
      controller.awaitNextKey[OrderCancelled](orderId)

    "Start order with permission in daylight saving time gap" in:
      // Admission is shifted to the next valid local time
      assert(local("2021-03-28T03:00") == local("2021-03-28T04:00"))
      clock := local("2021-03-28T02:59")
      val eventId = eventWatch.lastAddedEventId
      val orderId = OrderId("🔹")
      controller.api.addOrder(FreshOrder(orderId, sundayWorkflow.path)).await(99.s).orThrow
      eventWatch.await[OrderAttached](_.key == orderId, after = eventId)
      assert(controllerState.idToOrder(orderId).isState[Fresh])
      assert(controllerState.idToOrder(orderId).position == Position(0))
      assert(orderToObstacles(orderId) ==
        Right(Set(waitingForAdmmission(local("2021-03-28T03:00")))))

      clock := local("2021-03-28T04:00")
      eventWatch.await[OrderFinished](_.key == orderId)

    "Start order with permission in daylight saving time gap (2)" in:
      clock := local("2021-03-28T04:59")
      val eventId = eventWatch.lastAddedEventId
      val orderId = OrderId("🔶")
      controller.api.addOrder(FreshOrder(orderId, sundayWorkflow.path)).await(99.s).orThrow
      eventWatch.await[OrderFinished](_.key == orderId, after = eventId)

    "Start order at end of shifted permission period" in:
      clock := local("2021-03-28T05:00")
      val orderId = OrderId("🔷")
      controller.api.addOrder(FreshOrder(orderId, sundayWorkflow.path)).await(99.s).orThrow
      assert(controllerState.idToOrder(orderId).isState[Fresh])
      assert(controllerState.idToOrder(orderId).position == Position(0))
      assert(orderToObstacles(orderId) ==
        Right(Set(waitingForAdmmission(local("2021-04-04T03:00")))))
      execCmd:
        CancelOrders(orderId :: Nil)
      controller.awaitNextKey[OrderCancelled](orderId)
  }

  "Monday after end of daylight saving time" in:
    assert(local("2021-10-31T04:00") - local("2021-10-31T03:59") == 1.h + 1.minute)

    clock := local("2021-10-31T00:00")
    val eventId = eventWatch.lastAddedEventId
    val orderId = OrderId("♣️")
    controller.api.addOrder(FreshOrder(orderId, mondayWorkflow.path)).await(99.s).orThrow
    eventWatch.await[OrderAttached](_.key == orderId)
    assert(controllerState.idToOrder(orderId).isState[Fresh])
    assert(controllerState.idToOrder(orderId).position == Position(0))

    clock := local("2021-11-01T07:59")
    awaitAndAssert(1.s)(controllerState.idToOrder(orderId).isState[Fresh])
    assert(controllerState.idToOrder(orderId).position == Position(0))

    clock := local("2021-11-01T08:00")
    eventWatch.await[OrderFinished](_.key == orderId, after = eventId)

  "Late start" in:
    val tooEarly = local("2021-11-08T07:59")
    assert(!mondayAdmissionTimeScheme.isPermitted(tooEarly, dateOffset = 0.s))
    clock := tooEarly
    val eventId = eventWatch.lastAddedEventId
    val orderId = OrderId("♠️")
    controller.api.addOrder(FreshOrder(orderId, mondayWorkflow.path)).await(99.s).orThrow
    eventWatch.await[OrderAttached](_.key == orderId, after = eventId)
    assert(controllerState.idToOrder(orderId).isState[Fresh])
    assert(controllerState.idToOrder(orderId).position == Position(0))

    // Let the clock skip the permission interval until it is too late.
    // This may happen due to any delay, for example due to other other orders in the job.
    assert(mondayAdmissionTimeScheme
      .isPermitted(local("2021-11-08T08:00"), dateOffset = 0.s))

    val tooLate = local("2021-11-08T10:00")
    assert(!mondayAdmissionTimeScheme.isPermitted(tooLate, dateOffset = 0.s))
    clock := tooLate  // To late
    awaitAndAssert(1.s)(controllerState.idToOrder(orderId).isState[Fresh])
    assert(controllerState.idToOrder(orderId).position == Position(0))
    execCmd:
      CancelOrders(orderId :: Nil)
    controller.awaitNextKey[OrderCancelled](orderId)

  "forceJobAdmission in AddOrder command" in:
    // Test only inheritance to forked child orders
    val workflow = Workflow(WorkflowPath("forceJobAdmission"),
      Seq(
        Fork.forTest(Seq(
          "FORKED" -> Workflow.of(
            Execute(WorkflowJob(agentPath, EmptyJob.executable(),
              admissionTimeScheme = Some(sundayAdmissionTimeScheme))))))),
      timeZone = Timezone(zoneId.getId))

    withItem(workflow) { workflow =>
      clock := local("2023-06-21T00:00")

      val aOrderId = OrderId("♦️")
      val forkedaOrderId = aOrderId / "FORKED"
      locally:
        // Job is closed
        val eventId = eventWatch.lastAddedEventId
        controller.api.addOrder(FreshOrder(aOrderId, workflow.path)).await(99.s).orThrow
        eventWatch.await[OrderAttached](_.key == forkedaOrderId, after = eventId)
        assert(controllerState.idToOrder(aOrderId).isState[Order.Forked])
        assert(controllerState.idToOrder(forkedaOrderId).isState[Order.Ready])
        assert(orderToObstacles(forkedaOrderId) ==
          Right(Set(waitingForAdmmission(local("2023-06-25T03:00")))))

      locally:
        // Engine chooses this order due to forceJobAdmission despite it is not the first one in queue
        val orderId = OrderId("🥨")
        val eventId = eventWatch.lastAddedEventId
        controller.api.addOrder(FreshOrder(orderId, workflow.path, forceJobAdmission = true))
          .await(99.s).orThrow
        eventWatch.await[OrderFinished](_.key == orderId, after = eventId)

      assert(orderToObstacles(forkedaOrderId) ==
        Right(Set(waitingForAdmmission(local("2023-06-25T03:00")))))
      execCmd:
        CancelOrders(Seq(aOrderId, forkedaOrderId))
      controller.awaitKey[OrderFailed](aOrderId)  // Failed because child order cancelled
    }

  "forceJobAdmission in AddOrder instruction" in:
    clock := local("2023-06-22T00:00")

    val startingWorkflow1 = Workflow(WorkflowPath("forceJobAdmission-1"), Seq(
      AddOrder(StringConstant("FORCED-ORDER-1"), sundayWorkflow.path)))
    withItem(startingWorkflow1) { _ =>
      val eventId = eventWatch.lastAddedEventId
      controller.api.addOrder(FreshOrder(OrderId("STARTING-ORDER-1"), startingWorkflow1.path))
        .await(99.s).orThrow

      val orderId = OrderId("FORCED-ORDER-1")
      eventWatch.await[OrderAttached](_.key == orderId, after = eventId)
      assert(orderToObstacles(orderId) ==
        Right(Set(waitingForAdmmission(local("2023-06-25T03:00")))))
      execCmd:
        CancelOrders(Seq(orderId))
      controller.awaitNextKey[OrderCancelled](orderId)
    }

    val startingWorkflow2 = Workflow(WorkflowPath("forceJobAdmission-2"), Seq(
      AddOrder(StringConstant("FORCED-ORDER-2"), sundayWorkflow.path, forceJobAdmission = true)))
    withItem(startingWorkflow2) { _ =>
      val eventId = eventWatch.lastAddedEventId
      controller.api.addOrder(FreshOrder(OrderId("STARTING-ORDER-2"), startingWorkflow2.path))
        .await(99.s).orThrow

      val orderId = OrderId("FORCED-ORDER-2")
      eventWatch.await[OrderProcessingStarted](_.key == orderId, after = eventId)
      eventWatch.await[OrderFinished](_.key == orderId, after = eventId)
    }

  "killAtEndOfAdmissionPeriod" - {
    "single AdmissionPeriod" in:
      controller.resetLastWatchedEventId()
      val workflow = Workflow(
        WorkflowPath("killAtEndOfAdmissionPeriod"),
        Seq:
          Execute(WorkflowJob(agentPath,
            ShellScriptExecutable(
            s"""#!/usr/bin/env bash
              |set -euo pipefail
              |echo Hej!
              |while true; do :
              |  sleep 0.1
              |done
              |exit 1
              |""".stripMargin),
            admissionTimeScheme = Some(AdmissionTimeScheme(Seq(
              WeekdayPeriod(MONDAY, LocalTime.of(8, 0), 2.h)))),
            killAtEndOfAdmissionPeriod = true)),
        timeZone = Timezone(zoneId.getId))
      withItem(workflow): workflow =>
        clock := local("2025-06-30T00:00")
        val orderId = OrderId("killAtEndOfAdmissionPeriod-1")
        controller.api.addOrder(FreshOrder(orderId, workflow.path))
          .await(99.s).orThrow
        controller.awaitNextKey[OrderAttached](orderId)
        assert(orderToObstacles(orderId) == Right(Set(waitingForAdmmission(local("2025-06-30T08:00")))))

        clock := local("2025-06-30T09:00")
        controller.awaitNextKey[OrderProcessingStarted](orderId)
        controller.awaitNextKey[OrderStdoutWritten](orderId)

        clock := local("2025-06-30T10:00")
        assert(controllerState.idToOrder(orderId).isState[Order.Processing])
        controller.awaitNextKey[OrderFailed](orderId)

    "Two periods without a gap" in:
      val delay = 100.ms
      controller.resetLastWatchedEventId()
      val workflow = Workflow(
        WorkflowPath("killAtEndOfAdmissionPeriod"),
        Seq:
          Execute(WorkflowJob(agentPath,
            ShellScriptExecutable(
            s"""#!/usr/bin/env bash
              |set -euo pipefail
              |echo Hej!
              |while true; do :
              |  sleep ${delay.toBigDecimalSeconds}
              |done
              |exit 1
              |""".stripMargin),
            admissionTimeScheme = Some(AdmissionTimeScheme(Seq(
              WeekdayPeriod(MONDAY, LocalTime.of(8, 0), 2.h),
              SpecificDatePeriod(LocalDateTime.parse("2025-06-30T10:00"), 30.minutes)))),
            killAtEndOfAdmissionPeriod = true)),
        timeZone = Timezone(zoneId.getId))
      withItem(workflow): workflow =>
        clock := local("2025-06-30T00:00")
        val orderId = OrderId("killAtEndOfAdmissionPeriod-2")
        controller.api.addOrder(FreshOrder(orderId, workflow.path)).await(99.s).orThrow
        controller.awaitNextKey[OrderAttached](orderId)
        assert(orderToObstacles(orderId) == Right(Set(waitingForAdmmission(local("2025-06-30T08:00")))))

        clock := local("2025-06-30T09:00")
        controller.awaitNextKey[OrderProcessingStarted](orderId)
        controller.awaitNextKey[OrderStdoutWritten](orderId)

        clock := local("2025-06-30T10:00")
        sleep(2 * delay) // Time-dependent !!!
        assert(controllerState.idToOrder(orderId).state.isInstanceOf[Order.Processing])

        clock := local("2025-06-30T10:30")
        controller.awaitNextKey[OrderFailed](orderId)
  }

object JobAdmissionTimeTest:

  private val agentPath = AgentPath("AGENT")
  private val zoneId = ZoneId.of("Europe/Mariehamn")

  private val mondayAdmissionTimeScheme = AdmissionTimeScheme(Seq(
    WeekdayPeriod(MONDAY, LocalTime.of(8, 0), 2.h)))

  private val mondayWorkflow = Workflow(WorkflowPath("MONDAY") ~ "INITIAL",
    Seq(
      Execute(WorkflowJob(agentPath, EmptyJob.executable(),
        admissionTimeScheme = Some(mondayAdmissionTimeScheme)))),
    timeZone = Timezone(zoneId.getId))

  private val sundayAdmissionTimeScheme = AdmissionTimeScheme(Seq(
    WeekdayPeriod(SUNDAY, LocalTime.of(3, 0), 1.h) /*dst gap*/))

  private val sundayWorkflow = Workflow(WorkflowPath("SUNDAY") ~ "INITIAL",
    Seq(
      Execute(WorkflowJob(agentPath, EmptyJob.executable(),
        admissionTimeScheme = Some(sundayAdmissionTimeScheme)))),
    timeZone = Timezone(zoneId.getId))
