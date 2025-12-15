package js7.tests

import js7.base.configutils.Configs.HoconStringInterpolator
import js7.base.test.OurTestSuite
import js7.base.thread.CatsBlocking.syntax.await
import js7.base.time.ScalaTime.*
import js7.base.time.TimestampForTests.ts
import js7.base.time.{TestAlarmClock, Timestamp}
import js7.base.utils.ScalaUtils.syntax.RichEither
import js7.controller.RunningController
import js7.data.agent.AgentPath
import js7.data.command.SuspensionMode
import js7.data.controller.ControllerCommand.{GoOrder, ResumeOrder, SuspendOrders}
import js7.data.order.OrderEvent.{OrderAdded, OrderAttachable, OrderAttached, OrderAwoke, OrderDeleted, OrderDetachable, OrderDetached, OrderFinished, OrderGoMarked, OrderGoes, OrderMoved, OrderProcessed, OrderProcessingStarted, OrderResumed, OrderSleeping, OrderStarted, OrderStateReset, OrderSuspended, OrderSuspensionMarked}
import js7.data.order.{FreshOrder, Order, OrderEvent, OrderId, OrderOutcome}
import js7.data.value.expression.ExpressionParser.expr
import js7.data.workflow.Workflow
import js7.data.workflow.instructions.Sleep
import js7.data.workflow.position.Position
import js7.subagent.Subagent
import js7.tests.SleepTest.*
import js7.tests.jobs.EmptyJob
import js7.tests.testenv.ControllerAgentForScalaTest
import js7.tests.testenv.DirectoryProvider.toLocalSubagentId

final class SleepTest extends OurTestSuite, ControllerAgentForScalaTest:

  override protected val controllerConfig = config"""
    js7.auth.users.TEST-USER.permissions = [ UpdateItem ]
    js7.journal.remove-obsolete-files = false
    js7.controller.agent-driver.command-batch-delay = 0ms
    js7.controller.agent-driver.event-buffer-delay = 0ms"""

  override protected def agentConfig = config"""
    js7.job.execution.signed-script-injection-allowed = on"""

  private lazy val clock = TestAlarmClock(ts"2024-12-18T12:00:00Z")

  override protected def controllerTestWiring = RunningController.TestWiring(
    alarmClock = Some(clock))

  override protected def subagentTestWiring = Subagent.TestWiring(
    clock = clock)

  protected def agentPaths = Seq(agentPath)
  protected def items = Nil

  "Sleep at start, at Controller" in:
    val eventId = eventWatch.resetLastWatchedEventId()
    val workflow = Workflow.of:
      Sleep(expr("3"))
    withItem(workflow): workflow =>
      val orderId = OrderId("ORDER-JOBLESS")
      controller.api.addOrder:
        FreshOrder(orderId, workflow.path, deleteWhenTerminated = true)
      .await(99.s).orThrow

      assert(controllerState.idToOrder(orderId).isState[Order.Sleeping])
      clock.tick(2.s)
      assert(controllerState.idToOrder(orderId).isState[Order.Sleeping])
      clock.tick(1.s)
      eventWatch.awaitNextKey[OrderFinished](orderId)

      assert(eventWatch.eventsByKey[OrderEvent](orderId) == Seq(
        OrderAdded(workflow.id, deleteWhenTerminated = true),
        OrderStarted,
        OrderSleeping(until = ts"2024-12-18T12:00:03Z"),
        OrderMoved(Position(1)),
        OrderFinished(),
        OrderDeleted))

  "Sleep zero seconds" in:
    clock.resetTo(ts"2024-12-18T12:00:00Z")
    val eventId = eventWatch.resetLastWatchedEventId()
    val workflow = Workflow.of(
      Sleep(expr("0")),
      Sleep(expr("-1")),
      Sleep(expr("missing")))

    withItem(workflow): workflow =>
      val orderId = OrderId("ORDER-ZERO")
      controller.api.addOrder:
        FreshOrder(orderId, workflow.path, deleteWhenTerminated = true)
      .await(99.s).orThrow

      eventWatch.awaitNextKey[OrderFinished](orderId)

      assert(eventWatch.eventsByKey[OrderEvent](orderId) == Seq(
        OrderAdded(workflow.id, deleteWhenTerminated = true),
        OrderStarted,
        OrderMoved(Position(1)),
        OrderMoved(Position(2)),
        OrderMoved(Position(3)),
        OrderFinished(),
        OrderDeleted))

  "Sleep after start, at Agent" in:
    clock.resetTo(ts"2024-12-18T13:00:00Z")
    val eventId = eventWatch.resetLastWatchedEventId()
    val workflow = Workflow.of(
      EmptyJob.execute(agentPath),
      Sleep(expr("3")))
    withItem(workflow): workflow =>
      val orderId = OrderId("ORDER-WITH-JOB")
      controller.api.addOrder:
        FreshOrder(orderId, workflow.path, deleteWhenTerminated = true)
      .await(99.s).orThrow

      clock.tick()
      eventWatch.awaitNextKey[OrderSleeping](orderId)
      clock.tick(2.s)
      assert(controllerState.idToOrder(orderId).isState[Order.Sleeping])
      clock.tick(1.s)
      eventWatch.awaitNextKey[OrderFinished](orderId)

      assert(eventWatch.eventsByKey[OrderEvent](orderId) == Seq(
        OrderAdded(workflow.id, deleteWhenTerminated = true),
        OrderAttachable(agentPath),
        OrderAttached(agentPath),
        OrderStarted,
        OrderProcessingStarted(subagentId),
        OrderProcessed(OrderOutcome.succeeded),
        OrderMoved(Position(1)),
        OrderSleeping(until = ts"2024-12-18T13:00:03Z"),
        OrderMoved(Position(2)),
        OrderDetachable,
        OrderDetached,
        OrderFinished(),
        OrderDeleted))

  "GoOrder" in:
    clock.resetTo(ts"2024-12-18T14:00:00Z")
    val eventId = eventWatch.resetLastWatchedEventId()
    val workflow = Workflow.of(
      EmptyJob.execute(agentPath),
      Sleep(expr("3")))
    withItem(workflow): workflow =>
      val orderId = OrderId("ORDER-GO")
      controller.api.addOrder:
        FreshOrder(orderId, workflow.path, deleteWhenTerminated = true)
      .await(99.s).orThrow

      clock.tick()
      eventWatch.awaitNextKey[OrderSleeping](orderId)
      clock.tick(1.s)
      execCmd:
        GoOrder(orderId, Position(1))
      eventWatch.awaitNextKey[OrderFinished](orderId)

      assert(eventWatch.eventsByKey[OrderEvent](orderId) == Seq(
        OrderAdded(workflow.id, deleteWhenTerminated = true),
        OrderAttachable(agentPath),
        OrderAttached(agentPath),
        OrderStarted,
        OrderProcessingStarted(subagentId),
        OrderProcessed(OrderOutcome.succeeded),
        OrderMoved(Position(1)),
        OrderSleeping(until = ts"2024-12-18T14:00:03Z"),
        OrderGoMarked(Position(1)),
        OrderGoes,
        OrderAwoke,
        OrderMoved(Position(2)),
        OrderDetachable,
        OrderDetached,
        OrderFinished(),
        OrderDeleted))

  "SuspendOrders with reset" in:
    clock.resetTo(ts"2024-12-18T15:00:00Z")
    val eventId = eventWatch.resetLastWatchedEventId()
    val workflow = Workflow.of(
      EmptyJob.execute(agentPath),
      Sleep(expr("3")))
    withItem(workflow): workflow =>
      val orderId = OrderId("ORDER-SUSPEND")
      controller.api.addOrder:
        FreshOrder(orderId, workflow.path, deleteWhenTerminated = true)
      .await(99.s).orThrow

      clock.tick()
      eventWatch.awaitNextKey[OrderSleeping](orderId)
      clock.tick(1.s)
      execCmd:
        SuspendOrders(orderId :: Nil, SuspensionMode(resetState = true))
      eventWatch.awaitNextKey[OrderSuspended](orderId)

      execCmd:
        ResumeOrder(orderId)
      clock.tick(3.s)
      eventWatch.awaitNextKey[OrderFinished](orderId)

      assert(eventWatch.eventsByKey[OrderEvent](orderId) == Seq(
        OrderAdded(workflow.id, deleteWhenTerminated = true),
        OrderAttachable(agentPath),
        OrderAttached(agentPath),
        OrderStarted,
        OrderProcessingStarted(subagentId),
        OrderProcessed(OrderOutcome.succeeded),
        OrderMoved(Position(1)),
        OrderSleeping(until = ts"2024-12-18T15:00:03Z"),
        OrderSuspensionMarked(SuspensionMode(resetState = true)),
        OrderDetachable,
        OrderDetached,
        OrderStateReset,
        OrderSuspended,
        OrderResumed(),
        OrderSleeping(until = ts"2024-12-18T15:00:04Z"),
        OrderMoved(Position(2)),
        OrderFinished(),
        OrderDeleted))


object SleepTest:
  private val agentPath = AgentPath("AGENT")
  private val subagentId = toLocalSubagentId(agentPath)
