package js7.tests

import java.time.{LocalDateTime, ZoneId}
import js7.base.configutils.Configs.HoconStringInterpolator
import js7.base.test.OurTestSuite
import js7.base.test.TestExtensions.autoSome
import js7.base.time.JavaTimeConverters.{AsScalaInstant, toTimezone}
import js7.base.time.JavaTimeLiterals.localTime
import js7.base.time.ScalaTime.*
import js7.base.time.{AdmissionTimeScheme, MonthlyDatePeriod, TestAlarmClock, Timestamp, Timezone}
import js7.controller.RunningController
import js7.data.agent.AgentPath
import js7.data.command.SuspensionMode
import js7.data.controller.ControllerCommand.{CancelOrders, GoOrder, ResumeOrder, SuspendOrders, TransferOrders}
import js7.data.order.Order.WaitingForAdmission
import js7.data.order.OrderEvent.{OrderAdded, OrderAttachable, OrderAttached, OrderCancelled, OrderDeleted, OrderDetachable, OrderDetached, OrderFinished, OrderGoes, OrderMoved, OrderProcessed, OrderProcessingStarted, OrderResumed, OrderResumptionMarked, OrderSaid, OrderStarted, OrderStateReset, OrderSuspended, OrderSuspensionMarked, OrderTransferred, OrderWaitingForAdmission}
import js7.data.order.{FreshOrder, Order, OrderEvent, OrderId, OrderOutcome}
import js7.data.value.Value.convenience.given
import js7.data.value.expression.Expression.expr
import js7.data.workflow.instructions.{AdmissionTime, Say}
import js7.data.workflow.position.BranchPath.syntax.*
import js7.data.workflow.position.{BranchId, Position}
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.subagent.Subagent
import js7.tester.ScalaTestUtils.awaitAndAssert
import js7.tests.AdmissionTimeTest.*
import js7.tests.jobs.EmptyJob
import js7.tests.testenv.ControllerAgentForScalaTest
import js7.tests.testenv.DirectoryProvider.toLocalSubagentId
import scala.concurrent.duration.*
import scala.language.implicitConversions

final class AdmissionTimeTest extends OurTestSuite, ControllerAgentForScalaTest:

  override protected val controllerConfig = config"""
    js7.auth.users.TEST-USER.permissions = [ UpdateItem ]
    js7.journal.remove-obsolete-files = false
    js7.controller.agent-driver.command-batch-delay = 0ms
    js7.controller.agent-driver.event-buffer-delay = 0ms
    """

  override protected def agentConfig = config"""
    js7.job.execution.signed-script-injection-allowed = on
    """

  private lazy val clock = TestAlarmClock(local("2025-10-14T00:00"))

  override protected def controllerTestWiring = RunningController.TestWiring(
    alarmClock = Some(clock))

  override protected def subagentTestWiring = Subagent.TestWiring(
    clock = clock)

  protected def agentPaths = Seq(agentPath)
  protected def items = Nil

  "AdmissionTime allows immediate entry while at Controller" in:
    clock.resetTo(local("2025-10-14T12:00"))
    withItem(standardWorkflow): workflow =>
      val orderId = addOrder(workflow.path)
      controller.awaitNextKey[OrderSaid](orderId)
      controller.awaitNextKey[OrderFinished](orderId)

      assert(eventWatch.eventsByKey[OrderEvent](orderId) == Seq(
        OrderAdded(workflow.id, deleteWhenTerminated = true),
        OrderMoved(Position(0) / BranchId.AdmissionTime % 0),
        OrderStarted,
        OrderSaid("OK"),
        OrderMoved(Position(1)),
        OrderFinished(),
        OrderDeleted))

  "AdmissionTime allows immediate entry while at Agent" in:
    clock.resetTo(local("2025-10-14T12:00"))
    withItem(agentWorkflow): workflow =>
      val orderId = addOrder(workflow.path)
      controller.awaitNextKey[OrderSaid](orderId)
      controller.awaitNextKey[OrderFinished](orderId)

      assert(eventWatch.eventsByKey[OrderEvent](orderId) == Seq(
        OrderAdded(workflow.id, deleteWhenTerminated = true),
        OrderAttachable(agentPath),
        OrderAttached(agentPath),
        OrderStarted,
        OrderProcessingStarted(subagentId),
        OrderProcessed(OrderOutcome.succeeded),
        OrderMoved(Position(1)),
        OrderDetachable,
        OrderDetached,
        OrderMoved(Position(1) / BranchId.AdmissionTime % 0),
        OrderSaid("OK"),
        OrderMoved(Position(2)),
        OrderFinished(),
        OrderDeleted))

  "AdmissionTime waits for admission while at Controller" in:
    clock.resetTo(local("2025-10-14T00:00"))
    withItem(standardWorkflow): workflow =>
      val orderId = addOrder(workflow.path)
      controller.awaitNextKey[OrderWaitingForAdmission](orderId)

      clock := local("2025-10-14T11:59:00")
      awaitAndAssert(controllerState.idToOrder(orderId).isState[WaitingForAdmission])

      clock := local("2025-10-14T12:00:00")
      controller.awaitNextKey[OrderSaid](orderId)
      controller.awaitNextKey[OrderFinished](orderId)

      assert(eventWatch.eventsByKey[OrderEvent](orderId) == Seq(
        OrderAdded(workflow.id, deleteWhenTerminated = true),
        OrderStarted,
        OrderWaitingForAdmission(local("2025-10-14T12:00:00")),
        OrderMoved(Position(0) / BranchId.AdmissionTime % 0),
        OrderSaid("OK"),
        OrderMoved(Position(1)),
        OrderFinished(),
        OrderDeleted))

  "AdmissionTime wait for admission while at Agent" in:
    clock.resetTo(local("2025-10-14T00:00"))
    withItem(agentWorkflow): workflow =>
      val orderId = addOrder(workflow.path)
      controller.awaitNextKey[OrderWaitingForAdmission](orderId)

      clock := local("2025-10-14T11:59:00")
      awaitAndAssert(controllerState.idToOrder(orderId).isState[WaitingForAdmission])

      clock := local("2025-10-14T12:00:00")
      controller.awaitNextKey[OrderSaid](orderId)
      controller.awaitNextKey[OrderFinished](orderId)

      assert(eventWatch.eventsByKey[OrderEvent](orderId) == Seq(
        OrderAdded(workflow.id, deleteWhenTerminated = true),
        OrderAttachable(agentPath),
        OrderAttached(agentPath),
        OrderStarted,
        OrderProcessingStarted(subagentId),
        OrderProcessed(OrderOutcome.succeeded),
        OrderMoved(Position(1)),
        OrderDetachable,
        OrderDetached,
        OrderWaitingForAdmission(local("2025-10-14T12:00:00")),
        OrderMoved(Position(1) / BranchId.AdmissionTime % 0),
        OrderSaid("OK"),
        OrderMoved(Position(2)),
        OrderFinished(),
        OrderDeleted))

  "skipIfNoAdmissionStartForOrderDay, skip because order day has no admission start" in:
    clock.resetTo(local("2025-10-20T12:00"))
    val workflow = Workflow(
      WorkflowPath.Anonymous,
      timeZone = zoneId.toTimezone,
      instructions = Seq:
        AdmissionTime(
          AdmissionTimeScheme(Seq:
            MonthlyDatePeriod(21, localTime"12:00", 1.minute)),
          skipIfNoAdmissionStartForOrderDay = true
        ):
          Workflow.of:
            Say(expr"'OK'"))

    withItem(workflow): workflow =>
      val orderId = OrderId("#2025-10-20#ORDER")
      addOrder(orderId, workflow.path)
      controller.awaitNextKey[OrderFinished](orderId)

      assert(eventWatch.eventsByKey[OrderEvent](orderId) == Seq(
        OrderAdded(workflow.id, deleteWhenTerminated = true),
        OrderMoved(Position(1), OrderMoved.NoAdmissionPeriodStart),
        OrderStarted,
        OrderFinished(),
        OrderDeleted))

  "skipIfNoAdmissionStartForOrderDay, don't skip because order day has admission start" in :
    clock.resetTo(local("2025-10-21T00:00"))
    val workflow = Workflow(
      WorkflowPath.Anonymous,
      timeZone = zoneId.toTimezone,
      instructions = Seq:
        AdmissionTime(
          AdmissionTimeScheme(Seq:
            MonthlyDatePeriod(21, localTime"12:00", 1.minute)),
          skipIfNoAdmissionStartForOrderDay = true
        ):
          Workflow.of:
            Say(expr"'OK'"))

    withItem(workflow): workflow =>
      val orderId = OrderId("#2025-10-21#ORDER")
      addOrder(orderId, workflow.path)
      controller.awaitNextKey[OrderWaitingForAdmission](orderId)

      clock := local("2025-10-21T12:00:00")
      controller.awaitNextKey[OrderFinished](orderId)
      assert(eventWatch.eventsByKey[OrderEvent](orderId) == Seq(
        OrderAdded(workflow.id, deleteWhenTerminated = true),
        OrderStarted,
        OrderWaitingForAdmission(local("2025-10-21T12:00:00")),
        OrderMoved(Position(0) / BranchId.AdmissionTime % 0),
        OrderSaid("OK"),
        OrderMoved(Position(1)),
        OrderFinished(),
        OrderDeleted))

  "skipIfNoAdmissionStartForOrderDay, don't skip due to forceAdmission" in :
    clock.resetTo(local("2025-10-21T00:00"))
    val workflow = Workflow(
      WorkflowPath.Anonymous,
      timeZone = zoneId.toTimezone,
      instructions = Seq:
        AdmissionTime(
          AdmissionTimeScheme(Seq:
            MonthlyDatePeriod(19, localTime"12:00", 1.minute)),
          skipIfNoAdmissionStartForOrderDay = true
        ):
          Workflow.of:
            Say(expr"'OK'"))

    withItem(workflow): workflow =>
      val orderId = OrderId("#2025-10-21#SKIP-FORCE")
      addOrder:
        FreshOrder(orderId, workflow.path, forceAdmission = true)
      controller.awaitNextKey[OrderFinished](orderId)

      assert(eventWatch.eventsByKey[OrderEvent](orderId) == Seq(
        OrderAdded(workflow.id, forceAdmission = true, deleteWhenTerminated = true),
        OrderMoved(Position(0) / BranchId.AdmissionTime % 0),
        OrderStarted,
        OrderSaid("OK"),
        OrderMoved(Position(1)),
        OrderFinished(),
        OrderDeleted))

  "Order forceAdmission" in:
    clock.resetTo(local("2025-10-21T00:00"))
    withItem(standardWorkflow): workflow =>
      val orderId = nextOrderId()
      addOrder:
        FreshOrder(orderId, workflow.path, forceAdmission = true)
      controller.awaitNextKey[OrderSaid](orderId)
      controller.awaitNextKey[OrderFinished](orderId)

      assert(eventWatch.eventsByKey[OrderEvent](orderId) == Seq(
        OrderAdded(workflow.id, forceAdmission = true, deleteWhenTerminated = true),
        OrderMoved(Position(0) / BranchId.AdmissionTime % 0),
        OrderStarted,
        OrderSaid("OK"),
        OrderMoved(Position(1)),
        OrderFinished(),
        OrderDeleted))

  "Suspend and resume while waiting for with AdmissionTime" in:
    clock.resetTo(local("2025-10-14T00:00"))
    withItem(standardWorkflow): workflow =>
      val orderId = addOrder(workflow.path)
      clock.tick()
      eventWatch.awaitNextKey[OrderWaitingForAdmission](orderId)
      clock.tick(1.s)
      awaitAndAssert:
        controllerState.idToOrder(orderId).isState[Order.WaitingForAdmission]
      execCmd:
        SuspendOrders(Seq(orderId))
      controller.awaitNextKey[OrderSuspensionMarked](orderId)
      execCmd:
        ResumeOrder(orderId)
      controller.awaitNextKey[OrderResumptionMarked](orderId)
      awaitAndAssert:
        controllerState.idToOrder(orderId).isState[Order.WaitingForAdmission]

      clock := local("2025-10-14T11:59:00")
      assert:
        controllerState.idToOrder(orderId).isState[Order.WaitingForAdmission]

      clock := local("2025-10-14T12:00:00")
      controller.awaitNextKey[OrderSaid](orderId)
      controller.awaitNextKey[OrderFinished](orderId)

      assert(eventWatch.eventsByKey[OrderEvent](orderId) == Seq(
        OrderAdded(workflow.id, deleteWhenTerminated = true),
        OrderStarted,
        OrderWaitingForAdmission(local("2025-10-14T12:00:00")),
        OrderSuspensionMarked(),
        OrderResumptionMarked(),
        OrderMoved(Position(0) / BranchId.AdmissionTime % 0),
        OrderSaid("OK"),
        OrderMoved(Position(1)),
        OrderFinished(),
        OrderDeleted))

  "Suspend(resetState) and resume while waiting for with AdmissionTime" in:
    clock.resetTo(local("2025-10-14T00:00"))
    withItem(standardWorkflow): workflow =>
      val orderId = addOrder(workflow.path)
      clock.tick()
      eventWatch.awaitNextKey[OrderWaitingForAdmission](orderId)
      clock.tick(1.s)
      awaitAndAssert:
        controllerState.idToOrder(orderId).isState[Order.WaitingForAdmission]
      execCmd:
        SuspendOrders(Seq(orderId), SuspensionMode(resetState = true))
      controller.awaitNextKey[OrderSuspended](orderId)
      execCmd:
        ResumeOrder(orderId)
      controller.awaitNextKey[OrderResumed](orderId)
      awaitAndAssert:
        controllerState.idToOrder(orderId).isState[Order.WaitingForAdmission]

      clock := local("2025-10-14T11:59:00")
      assert:
        controllerState.idToOrder(orderId).isState[Order.WaitingForAdmission]

      clock := local("2025-10-14T12:00:00")
      controller.awaitNextKey[OrderSaid](orderId)
      controller.awaitNextKey[OrderFinished](orderId)

      assert(eventWatch.eventsByKey[OrderEvent](orderId) == Seq(
        OrderAdded(workflow.id, deleteWhenTerminated = true),
        OrderStarted,
        OrderWaitingForAdmission(local("2025-10-14T12:00:00")),
        OrderStateReset,
        OrderSuspended,
        OrderResumed(),
        OrderWaitingForAdmission(local("2025-10-14T12:00:00")),
        OrderMoved(Position(0) / BranchId.AdmissionTime % 0),
        OrderSaid("OK"),
        OrderMoved(Position(1)),
        OrderFinished(),
        OrderDeleted))

  "Suspend while waiting for AdmissionTime and resume after admission" in:
    clock.resetTo(local("2025-10-14T00:00"))
    withItem(standardWorkflow): workflow =>
      val orderId = addOrder(workflow.path)
      clock.tick()
      eventWatch.awaitNextKey[OrderWaitingForAdmission](orderId)
      clock.tick(1.s)
      awaitAndAssert:
        controllerState.idToOrder(orderId).isState[Order.WaitingForAdmission]
      execCmd:
        SuspendOrders(Seq(orderId))
      controller.awaitNextKey[OrderSuspensionMarked](orderId)
      clock.tick(1.s)
      awaitAndAssert:
        controllerState.idToOrder(orderId).isState[Order.WaitingForAdmission]
      clock := local("2025-10-14T12:00:00")
      controller.awaitNextKey[OrderSuspended](orderId)
      execCmd:
        ResumeOrder(orderId)
      controller.awaitNextKey[OrderResumed](orderId)

      controller.awaitNextKey[OrderSaid](orderId)
      controller.awaitNextKey[OrderFinished](orderId)

      assert(eventWatch.eventsByKey[OrderEvent](orderId) == Seq(
        OrderAdded(workflow.id, deleteWhenTerminated = true),
        OrderStarted,
        OrderWaitingForAdmission(local("2025-10-14T12:00:00")),
        OrderSuspensionMarked(),
        OrderMoved(Position(0) / BranchId.AdmissionTime % 0),
        OrderSuspended,
        OrderResumed(),
        OrderSaid("OK"),
        OrderMoved(Position(1)),
        OrderFinished(),
        OrderDeleted))

  "GoOrder" in:
    clock.resetTo(local("2025-10-14T00:00"))
    withItem(standardWorkflow): workflow =>
      val orderId = addOrder(workflow.path)
      clock.tick()
      eventWatch.awaitNextKey[OrderWaitingForAdmission](orderId)
      execCmd:
        GoOrder(orderId, Position(0))
      controller.awaitNextKey[OrderSaid](orderId)
      controller.awaitNextKey[OrderFinished](orderId)

      assert(eventWatch.eventsByKey[OrderEvent](orderId) == Seq(
        OrderAdded(workflow.id, deleteWhenTerminated = true),
        OrderStarted,
        OrderWaitingForAdmission(local("2025-10-14T12:00:00")),
        OrderGoes,
        OrderMoved(Position(0) / BranchId.AdmissionTime % 0),
        OrderSaid("OK"),
        OrderMoved(Position(1)),
        OrderFinished(),
        OrderDeleted))

  "TransferOrders" in:
    clock.resetTo(local("2025-10-14T00:00"))

    withItem(standardWorkflow): workflow =>
      val orderId = addOrder(workflow.path)
      eventWatch.awaitNextKey[OrderWaitingForAdmission](orderId)

      val newWorkflow = Workflow(
        workflow.path,
        timeZone = zoneId.toTimezone,
        instructions = Seq:
          AdmissionTime(AdmissionTimeScheme.daily(localTime"18:00", 1.minute)):
            Workflow.of:
              Say(expr"'OK'"))
      withItem(newWorkflow): newWorkflow =>
        execCmd:
          TransferOrders(workflow.id)
        eventWatch.awaitNextKey[OrderStateReset](orderId)
        eventWatch.awaitNextKey[OrderTransferred](orderId)
        eventWatch.awaitNextKey[OrderWaitingForAdmission](orderId)

        clock := local("2025-10-14T18:00:00")
        controller.awaitNextKey[OrderSaid](orderId)
        controller.awaitNextKey[OrderFinished](orderId)

        assert(eventWatch.eventsByKey[OrderEvent](orderId) == Seq(
          OrderAdded(workflow.id, deleteWhenTerminated = true),
          OrderStarted,
          OrderWaitingForAdmission(local("2025-10-14T12:00:00")),
          OrderStateReset,
          OrderTransferred(newWorkflow.id /: Position(0)),
          OrderWaitingForAdmission(local("2025-10-14T18:00:00")),
          OrderMoved(Position(0) / BranchId.AdmissionTime % 0),
          OrderSaid("OK"),
          OrderMoved(Position(1)),
          OrderFinished(),
          OrderDeleted))

  "Cancel while waiting for AdmissionTime" in:
    clock.resetTo(local("2025-10-21T00:00"))
    withItem(standardWorkflow): workflow =>
      val orderId = addOrder(workflow.path)
      clock.tick()
      eventWatch.awaitNextKey[OrderWaitingForAdmission](orderId)
      clock.tick(1.s)
      awaitAndAssert:
        controllerState.idToOrder(orderId).isState[Order.WaitingForAdmission]
      execCmd:
        CancelOrders(Seq(orderId))
      controller.awaitNextKey[OrderCancelled](orderId)

      assert(eventWatch.eventsByKey[OrderEvent](orderId) == Seq(
        OrderAdded(workflow.id, deleteWhenTerminated = true),
        OrderStarted,
        OrderWaitingForAdmission(local("2025-10-21T12:00:00")),
        OrderStateReset,
        OrderCancelled,
        OrderDeleted))


object AdmissionTimeTest:
  private val agentPath = AgentPath("AGENT")
  private val subagentId = toLocalSubagentId(agentPath)
  private val zoneId = ZoneId.of("Europe/Mariehamn")

  private def local(ymdhms: String): Timestamp =
    LocalDateTime.parse(ymdhms).atZone(zoneId).toInstant.toTimestamp

  private val admissionTimeScheme = AdmissionTimeScheme.daily(localTime"12:00", 1.minute)

  private val standardWorkflow = Workflow(
    WorkflowPath.Anonymous,
    timeZone = zoneId.toTimezone,
    instructions = Seq:
      AdmissionTime(admissionTimeScheme):
        Workflow.of:
          Say(expr"'OK'"))

  private val agentWorkflow = Workflow(
    WorkflowPath.Anonymous,
    timeZone = zoneId.toTimezone,
    instructions = Seq(
      EmptyJob.execute(agentPath),
      AdmissionTime(admissionTimeScheme):
        Workflow.of:
          Say(expr"'OK'")))
