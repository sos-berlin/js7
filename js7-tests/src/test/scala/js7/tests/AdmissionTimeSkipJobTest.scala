package js7.tests

import java.time.DayOfWeek.{FRIDAY, THURSDAY}
import java.time.{LocalTime, ZoneId}
import js7.agent.RunningAgent
import js7.base.configutils.Configs.HoconStringInterpolator
import js7.base.test.OurTestSuite
import js7.base.thread.CatsBlocking.syntax.*
import js7.base.time.JavaTimestamp.local
import js7.base.time.ScalaTime.*
import js7.base.time.TimestampForTests.ts
import js7.base.time.{AdmissionTimeScheme, TestAlarmClock, Timezone, WeekdayPeriod}
import js7.base.utils.ScalaUtils.syntax.RichEither
import js7.controller.RunningController
import js7.data.agent.AgentPath
import js7.data.execution.workflow.instructions.ExecuteExecutor.orderIdToDate
import js7.data.order.Order.Fresh
import js7.data.order.OrderEvent.{OrderAdded, OrderAttachable, OrderAttached, OrderDetachable, OrderDetached, OrderFinished, OrderMoved, OrderProcessed, OrderProcessingStarted, OrderStarted}
import js7.data.order.OrderObstacle.WaitingForAdmission
import js7.data.order.{FreshOrder, Order, OrderId, OrderOutcome}
import js7.data.workflow.instructions.Execute
import js7.data.workflow.instructions.executable.WorkflowJob
import js7.data.workflow.position.Position
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.tests.AdmissionTimeSkipJobTest.*
import js7.tests.jobs.EmptyJob
import js7.tests.testenv.ControllerAgentForScalaTest
import js7.tests.testenv.DirectoryProvider.toLocalSubagentId

final class AdmissionTimeSkipJobTest extends OurTestSuite, ControllerAgentForScalaTest:

  override protected val controllerConfig = config"""
    js7.auth.users.TEST-USER.permissions = [ UpdateItem ]
    js7.journal.remove-obsolete-files = false
    js7.controller.agent-driver.command-batch-delay = 0ms
    js7.controller.agent-driver.event-buffer-delay = 0ms"""

  override protected def agentConfig = config"""
    js7.job.execution.signed-script-injection-allowed = on"""

  protected def agentPaths = Seq(agentPath)
  protected def items = Seq(singleJobWorkflow, multipleJobsWorkflow)

  private implicit val timeZone: ZoneId = AdmissionTimeSkipJobTest.timeZone
  private given clock: TestAlarmClock = TestAlarmClock(local("2021-09-09T00:00"))

  override protected def controllerTestWiring = RunningController.TestWiring(
    alarmClock = Some(clock))

  override protected def agentTestWiring = RunningAgent.TestWiring(
    alarmClock = Some(clock))

  "Skip job if it has no admission time for order date" - {
    "Single job" in:
      val orderId = OrderId("#2021-09-02#single-job")
      assert(orderIdToDate(orderId).map(_.getDayOfWeek) == Some(THURSDAY))

      val events = controller.runOrder(FreshOrder(orderId, singleJobWorkflow.path)).map(_.value)
      assert(events == Seq(
        OrderAdded(singleJobWorkflow.id),
        OrderMoved(Position(1), Some(OrderMoved.NoAdmissionPeriodStart)),
        // Order does not start for skipped order (but for OrderFinished)
        OrderStarted,
        OrderFinished()))

    "Between other jobs" in:
      val orderId = OrderId("#2021-09-02#multiple-jobs")
      assert(orderIdToDate(orderId).map(_.getDayOfWeek) == Some(THURSDAY))

      val events = controller.runOrder(FreshOrder(orderId, multipleJobsWorkflow.path)).map(_.value)
      assert(events == Seq(
        OrderAdded(multipleJobsWorkflow.id),

        OrderAttachable(agentPath),
        OrderAttached(agentPath),

        OrderStarted,
        OrderProcessingStarted(subagentId),
        OrderProcessed(OrderOutcome.succeeded),
        OrderMoved(Position(1)),
        OrderMoved(Position(2), Some(OrderMoved.NoAdmissionPeriodStart)),
        OrderMoved(Position(3), Some(OrderMoved.NoAdmissionPeriodStart)),

        OrderProcessingStarted(subagentId),
        OrderProcessed(OrderOutcome.succeeded),
        OrderMoved(Position(4)),

        OrderDetachable,
        OrderDetached,
        OrderFinished()))
  }

  "Do not skip if job has a admission time for order date" in:
    clock.resetTo(local("2021-09-03T00:00")) // Friday
    val eventId = eventWatch.lastAddedEventId
    val orderId = OrderId("#2021-09-03#")  // Friday
    assert(orderIdToDate(orderId).map(_.getDayOfWeek) == Some(FRIDAY))

    controller.api.addOrder(FreshOrder(orderId, singleJobWorkflow.path)).await(99.s).orThrow
    eventWatch.await[OrderAttached](_.key == orderId, after = eventId)
    sleep(50.ms)
    assert(controllerState.idToOrder(orderId).state == Order.Fresh())
    assert(controllerState.orderToObstacles(orderId) == Right(Set:
      WaitingForAdmission(ts"2021-09-03T15:00:00Z")))

    clock := local("2021-09-03T18:00")
    eventWatch.await[OrderProcessed](_.key == orderId, after = eventId)
    eventWatch.await[OrderFinished](_.key == orderId, after = eventId)

  "Do not skip if OrderId has no order date" in:
    clock := local("2021-09-10T00:00") // Friday
    val eventId = eventWatch.lastAddedEventId
    val orderId = OrderId("NO-DATE")
    controller.api.addOrder(FreshOrder(orderId, singleJobWorkflow.path)).await(99.s).orThrow
    eventWatch.await[OrderAttached](_.key == orderId, after = eventId)
    sleep(50.ms)
    assert(controllerState.idToOrder(orderId).state == Order.Fresh())
    assert(controllerState.orderToObstacles(orderId) == Right(Set:
      WaitingForAdmission(ts"2021-09-10T15:00:00Z")))

    clock := local("2021-09-10T18:00")
    eventWatch.await[OrderProcessed](_.key == orderId, after = eventId)
    eventWatch.await[OrderFinished](_.key == orderId, after = eventId)

  "Do not skip if OrderId has an invalid order date" in:
    clock := local("2021-09-20T00:00") // Friday
    val eventId = eventWatch.lastAddedEventId
    val orderId = OrderId("#2021-02-29#invalid")
    assert(orderIdToDate(orderId).map(_.getDayOfWeek) == None)

    controller.api.addOrder(FreshOrder(orderId, singleJobWorkflow.path)).await(99.s).orThrow
    eventWatch.await[OrderAttached](_.key == orderId, after = eventId)
    sleep(50.ms)
    assert(controllerState.idToOrder(orderId).state == Order.Fresh())
    assert(controllerState.orderToObstacles(orderId) == Right(Set:
      WaitingForAdmission(ts"2021-09-24T15:00:00Z")))

    clock := local("2021-09-24T17:59")
    sleep(50.ms)
    assert(controllerState.orderToObstacles(orderId) == Right(Set:
      WaitingForAdmission(ts"2021-09-24T15:00:00Z")))

    clock := local("2021-09-24T18:00")
    eventWatch.await[OrderProcessed](_.key == orderId, after = eventId)
    eventWatch.await[OrderFinished](_.key == orderId, after = eventId)


object AdmissionTimeSkipJobTest:

  private val agentPath = AgentPath("AGENT")
  private val subagentId = toLocalSubagentId(agentPath)
  private val timeZone = ZoneId.of("Europe/Mariehamn")

  private val fridayExecute = Execute(
    WorkflowJob(
      agentPath,
      EmptyJob.executable(),
      admissionTimeScheme = Some(AdmissionTimeScheme(Seq(
        WeekdayPeriod(FRIDAY, LocalTime.of(18, 0), 1.h)))),
      skipIfNoAdmissionStartForOrderDay = true))

  private val singleJobWorkflow = Workflow(WorkflowPath("SINGLE-JOB") ~ "INITIAL",
    Seq(fridayExecute),
    timeZone = Timezone(timeZone.getId))

  private val multipleJobsWorkflow = Workflow(WorkflowPath("MULTIPLE-JOBS") ~ "INITIAL",
    Seq(
      EmptyJob.execute(agentPath),
      fridayExecute,
      fridayExecute,
      EmptyJob.execute(agentPath)),
    timeZone = Timezone(timeZone.getId))
