package js7.tests

import js7.base.configutils.Configs.HoconStringInterpolator
import js7.base.io.process.ProcessSignal.{SIGKILL, SIGTERM}
import js7.base.io.process.ReturnCode
import js7.base.problem.Checked.Ops
import js7.base.system.OperatingSystem.isWindows
import js7.base.thread.MonixBlocking.syntax._
import js7.base.time.ScalaTime._
import js7.base.time.Timestamp
import js7.data.Problems.{CancelStartedOrderProblem, UnknownOrderProblem}
import js7.data.agent.AgentPath
import js7.data.command.CancellationMode
import js7.data.controller.ControllerCommand.{CancelOrders, Response}
import js7.data.item.VersionId
import js7.data.job.ShellScriptExecutable
import js7.data.order.OrderEvent.{OrderAdded, OrderAttachable, OrderAttached, OrderCancellationMarked, OrderCancellationMarkedOnAgent, OrderCancelled, OrderDetachable, OrderDetached, OrderFailed, OrderFinished, OrderForked, OrderJoined, OrderMoved, OrderProcessed, OrderProcessingKilled, OrderProcessingStarted, OrderStarted, OrderStdWritten, OrderStdoutWritten, OrderTerminated}
import js7.data.order.{FreshOrder, OrderEvent, OrderId, Outcome}
import js7.data.value.Value.convenience._
import js7.data.value.expression.Expression.NamedValue
import js7.data.value.{NamedValues, NumberValue}
import js7.data.workflow.instructions.executable.WorkflowJob
import js7.data.workflow.instructions.{Execute, Fork}
import js7.data.workflow.position.{Position, WorkflowPosition}
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.tests.CancelOrdersTest._
import js7.tests.testenv.ControllerAgentForScalaTest
import monix.execution.Scheduler.Implicits.global
import org.scalatest.freespec.AnyFreeSpec
import scala.concurrent.duration.Deadline.now
import scala.concurrent.duration._

/**
  * @author Joacim Zschimmer
  */
final class CancelOrdersTest extends AnyFreeSpec with ControllerAgentForScalaTest
{
  override protected val controllerConfig = config"""
    js7.controller.agent-driver.command-batch-delay = 0ms
    js7.controller.agent-driver.event-buffer-delay = 0ms"""

  override protected def agentConfig = config"""
    js7.job.execution.signed-script-injection-allowed = on
    js7.job.execution.sigkill-delay = 30s
    """

  protected val agentPaths = Seq(agentPath)
  protected val items = Seq(singleJobWorkflow, sigkillDelayWorkflow, sigkillImmediatelyWorkflow,
    twoJobsWorkflow, forkJoinIfFailedWorkflow, forkWorkflow)

  "Cancel a fresh order" in {
    val order = FreshOrder(OrderId("🔹"), singleJobWorkflow.path, scheduledFor = Some(Timestamp.now + 99.seconds))
    controller.addOrderBlocking(order)
    eventWatch.await[OrderAttached](_.key == order.id)
    controllerApi.executeCommand(CancelOrders(Set(order.id), CancellationMode.FreshOnly)).await(99.seconds).orThrow
    eventWatch.await[OrderCancelled](_.key == order.id)
    assert(eventWatch.keyedEvents[OrderEvent](order.id) == Vector(
      OrderAdded(singleJobWorkflow.id, order.arguments, order.scheduledFor),
      OrderAttachable(agentPath),
      OrderAttached(agentPath),
      OrderCancellationMarked(CancellationMode.FreshOnly),
      OrderDetachable,
      OrderDetached,
      OrderCancelled))
  }

  "Cancel a finishing order" in {
    val order = FreshOrder(OrderId("🔺"), singleJobWorkflow.path, Map("sleep" -> 1))
    controller.addOrderBlocking(order)
    eventWatch.await[OrderProcessingStarted](_.key == order.id)
    controllerApi.executeCommand(CancelOrders(Set(order.id), CancellationMode.FreshOrStarted())).await(99.seconds).orThrow
    eventWatch.await[OrderFinished](_.key == order.id)
    assert(eventWatch.keyedEvents[OrderEvent](order.id).filterNot(_.isInstanceOf[OrderStdWritten]) == Vector(
      OrderAdded(singleJobWorkflow.id, order.arguments, order.scheduledFor),
      OrderAttachable(agentPath),
      OrderAttached(agentPath),
      OrderStarted,
      OrderProcessingStarted,
      OrderCancellationMarked(CancellationMode.FreshOrStarted()),
      OrderCancellationMarkedOnAgent,
      OrderProcessed(Outcome.succeededRC0),
      OrderMoved(Position(1)),
      OrderDetachable,
      OrderDetached,
      OrderFinished))
  }

  "Cancelling (mode=FreshOnly) a started order is not possible" in {
    val order = FreshOrder(OrderId("❌"), twoJobsWorkflow.path, Map("sleep" -> 5))
    controller.addOrderBlocking(order)
    eventWatch.await[OrderProcessingStarted](_.key == order.id)
    sleep(100.ms)  // ControllerOrderKeeper may take some time to update its state
    // Controller knows, the order has started
    assert(controllerApi.executeCommand(CancelOrders(Set(order.id), CancellationMode.FreshOnly)).await(99.seconds) ==
      Left(CancelStartedOrderProblem(OrderId("❌"))))
    controllerApi.executeCommand(CancelOrders(Set(order.id), CancellationMode.FreshOrStarted(Some(CancellationMode.Kill()))))
      .await(99.seconds).orThrow
    eventWatch.await[OrderTerminated](_.key == order.id)
  }

  "Cancel a started order between two jobs" in {
    val order = FreshOrder(OrderId("🔴"), twoJobsWorkflow.path, Map("sleep" -> 2))
    controller.addOrderBlocking(order)
    eventWatch.await[OrderProcessingStarted](_.key == order.id)
    controllerApi.executeCommand(CancelOrders(Set(order.id), CancellationMode.FreshOrStarted())).await(99.seconds).orThrow
    eventWatch.await[OrderCancelled](_.key == order.id)
    assert(eventWatch.keyedEvents[OrderEvent](order.id).filterNot(_.isInstanceOf[OrderStdWritten]) == Vector(
      OrderAdded(twoJobsWorkflow.id, order.arguments, order.scheduledFor),
      OrderAttachable(agentPath),
      OrderAttached(agentPath),
      OrderStarted,
      OrderProcessingStarted,
      OrderCancellationMarked(CancellationMode.FreshOrStarted()),
      OrderCancellationMarkedOnAgent,
      OrderProcessed(Outcome.succeededRC0),
      OrderMoved(Position(1)),
      OrderDetachable,
      OrderDetached,
      OrderCancelled))
  }

  "Cancel an order and the first job" in {
    val order = FreshOrder(OrderId("⭕️"), singleJobWorkflow.path, Map("sleep" -> 100))
    testCancelFirstJob(order, Some(singleJobWorkflow.id /: Position(0)), immediately = false)
  }

  "Cancel an order but not the first job" in {
    val order = FreshOrder(OrderId("🔶"), twoJobsWorkflow.path, Map("sleep" -> 2))
    testCancel(order, Some(twoJobsWorkflow.id /: Position(1)), immediately = false,
      expectedEvents = mode => Vector(
        OrderAdded(twoJobsWorkflow.id, order.arguments),
        OrderAttachable(agentPath),
        OrderAttached(agentPath),
        OrderStarted,
        OrderProcessingStarted,
        OrderCancellationMarked(mode),
        OrderCancellationMarkedOnAgent,
        OrderProcessed(Outcome.succeededRC0),
        OrderMoved(Position(1)),
        OrderDetachable,
        OrderDetached,
        OrderCancelled))
  }

  "Cancel an order and the currently running job" in {
    val order = FreshOrder(OrderId("🔷"), singleJobWorkflow.path, Map("sleep" -> 100))
    testCancelFirstJob(order, None, immediately = false)
  }

  "Cancel an order and a certain running job with SIGKILL" in {
    val order = FreshOrder(OrderId("🔵"), singleJobWorkflow.path, Map("sleep" -> 100))
    testCancelFirstJob(order, Some(singleJobWorkflow.id /: Position(0)),
      immediately = true)
  }

  if (!isWindows) {
    "Cancel with sigkillDelay" in {
      val order = FreshOrder(OrderId("🟥️"), sigkillDelayWorkflow.path)
      val t = now
      testCancel(order, None, awaitTrapping = true, immediately = false,
        mode => Vector(
          OrderAdded(order.workflowPath ~ versionId, order.arguments),
          OrderAttachable(agentPath),
          OrderAttached(agentPath),
          OrderStarted,
          OrderProcessingStarted,
          OrderCancellationMarked(mode),
          OrderCancellationMarkedOnAgent,
          OrderProcessed(Outcome.Killed(Outcome.Failed(NamedValues.rc(ReturnCode(SIGKILL))))),
          OrderProcessingKilled,
          OrderDetachable,
          OrderDetached,
          OrderCancelled))
      assert(t.elapsed > sigkillDelay)
    }

    "Cancel with sigkillDelay=0s" in {
      val order = FreshOrder(OrderId("🟧️"), sigkillImmediatelyWorkflow.path)
      testCancel(order, None, awaitTrapping = true, immediately = false,
        mode => Vector(
          OrderAdded(order.workflowPath ~ versionId, order.arguments),
          OrderAttachable(agentPath),
          OrderAttached(agentPath),
          OrderStarted,
          OrderProcessingStarted,
          OrderCancellationMarked(mode),
          OrderCancellationMarkedOnAgent,
          OrderProcessed(Outcome.Killed(Outcome.Failed(NamedValues.rc(ReturnCode(SIGKILL))))),
          OrderProcessingKilled,
          OrderDetachable,
          OrderDetached,
          OrderCancelled))
    }
  }

  private def testCancelFirstJob(order: FreshOrder, workflowPosition: Option[WorkflowPosition], immediately: Boolean): Unit =
    testCancel(order, workflowPosition, immediately = immediately,
      expectedEvents = mode => Vector(
        OrderAdded(order.workflowPath ~ versionId, order.arguments),
        OrderAttachable(agentPath),
        OrderAttached(agentPath),
        OrderStarted,
        OrderProcessingStarted,
        OrderCancellationMarked(mode),
        OrderCancellationMarkedOnAgent,
        OrderProcessed(Outcome.Killed(Outcome.Failed(NamedValues.rc(
          if (isWindows) ReturnCode(0) else ReturnCode(if (immediately) SIGKILL else SIGTERM))))),
        OrderProcessingKilled,
        OrderDetachable,
        OrderDetached,
        OrderCancelled))

  "Cancel a forking order and kill job" in {
    val order = FreshOrder(OrderId("FORK"), forkWorkflow.path, Map("sleep" -> 2))
    controller.addOrderBlocking(order)
    eventWatch.await[OrderProcessingStarted](_.key == order.id / "🥕")

    val mode = CancellationMode.FreshOrStarted(Some(CancellationMode.Kill()))
    controllerApi.executeCommand(CancelOrders(Set(order.id), mode))
      .await(99.seconds).orThrow
    eventWatch.await[OrderCancelled](_.key == order.id)

    assert(controller.eventWatch
      .keyedEvents[OrderEvent]
      .filter(_.key.string startsWith "FORK")
      .filterNot(_.event.isInstanceOf[OrderStdWritten]) ==
      Vector(
        OrderId("FORK") <-: OrderAdded(forkWorkflow.id, order.arguments, order.scheduledFor),
        OrderId("FORK") <-: OrderStarted,
        OrderId("FORK") <-: OrderForked(Vector(OrderForked.Child(Fork.Branch.Id("🥕"), OrderId("FORK|🥕")))),
        OrderId("FORK|🥕") <-: OrderAttachable(agentPath),
        OrderId("FORK|🥕") <-: OrderAttached(agentPath),
        OrderId("FORK|🥕") <-: OrderProcessingStarted,
        OrderId("FORK") <-: OrderCancellationMarked(mode),
        OrderId("FORK|🥕") <-: OrderProcessed(Outcome.succeededRC0),
        OrderId("FORK|🥕") <-: OrderMoved(Position(0) / "fork+🥕" % 1),
        OrderId("FORK|🥕") <-: OrderDetachable,
        OrderId("FORK|🥕") <-: OrderDetached,
        OrderId("FORK") <-: OrderJoined(Outcome.succeeded),
        OrderId("FORK") <-: OrderMoved(Position(1)),
        OrderId("FORK") <-: OrderCancelled))
  }

  "Cancel a forked child order and kill job" in {
    val order = FreshOrder(OrderId("CANCEL-CHILD"), forkJoinIfFailedWorkflow.path, Map("sleep" -> 2))
    controller.addOrderBlocking(order)
    eventWatch.await[OrderProcessingStarted](_.key == order.id / "🥕")

    val mode = CancellationMode.FreshOrStarted(Some(CancellationMode.Kill()))
    controllerApi.executeCommand(CancelOrders(Set(order.id  / "🥕"), mode))
      .await(99.seconds).orThrow
    eventWatch.await[OrderCancelled](_.key == order.id / "🥕")

    assert(controller.eventWatch
      .keyedEvents[OrderEvent]
      .filter(_.key.string startsWith "CANCEL-CHILD")
      .filterNot(_.event.isInstanceOf[OrderStdWritten]) ==
      Vector(
        OrderId("CANCEL-CHILD") <-: OrderAdded(forkJoinIfFailedWorkflow.id, order.arguments,
          order.scheduledFor),
        OrderId("CANCEL-CHILD") <-: OrderStarted,
        OrderId("CANCEL-CHILD") <-: OrderForked(Vector(
          OrderForked.Child(Fork.Branch.Id("🥕"), OrderId("CANCEL-CHILD|🥕")))),
        OrderId("CANCEL-CHILD|🥕") <-: OrderAttachable(agentPath),
        OrderId("CANCEL-CHILD|🥕") <-: OrderAttached(agentPath),
        OrderId("CANCEL-CHILD|🥕") <-: OrderProcessingStarted,
        OrderId("CANCEL-CHILD|🥕") <-: OrderCancellationMarked(mode),
        OrderId("CANCEL-CHILD|🥕") <-: OrderCancellationMarkedOnAgent,
        OrderId("CANCEL-CHILD|🥕") <-: OrderProcessed(Outcome.Killed(
          Outcome.Failed(namedValues = Map("returnCode" -> NumberValue(128+15))))),
        OrderId("CANCEL-CHILD|🥕") <-: OrderProcessingKilled,
        OrderId("CANCEL-CHILD|🥕") <-: OrderDetachable,
        OrderId("CANCEL-CHILD|🥕") <-: OrderDetached,
        OrderId("CANCEL-CHILD|🥕") <-: OrderCancelled,
        OrderId("CANCEL-CHILD") <-: OrderJoined(
          Outcome.Failed(Some("Order:CANCEL-CHILD|🥕 has been cancelled"))),
        OrderId("CANCEL-CHILD") <-: OrderFailed(Position(0))))
  }

  private def testCancel(order: FreshOrder, workflowPosition: Option[WorkflowPosition],
    awaitTrapping: Boolean = false,
    immediately: Boolean,
    expectedEvents: CancellationMode => Seq[OrderEvent])
  : Unit = {
    controller.addOrderBlocking(order)
    eventWatch.await[OrderProcessingStarted](_.key == order.id)
    eventWatch.await[OrderStdoutWritten](_ == order.id <-: OrderStdoutWritten("READY\n"))
    val mode = CancellationMode.FreshOrStarted(Some(CancellationMode.Kill(
      immediately = immediately,
      workflowPosition)))
    controllerApi.executeCommand(CancelOrders(Set(order.id), mode))
      .await(99.seconds).orThrow
    eventWatch.await[OrderCancelled](_.key == order.id)
    assert(eventWatch.keyedEvents[OrderEvent](order.id)
      .filterNot(_.isInstanceOf[OrderStdWritten]) ==
      expectedEvents(mode))
  }

  "Cancel unknown order" in {
    assert(controllerApi.executeCommand(
      CancelOrders(Set(OrderId("UNKNOWN")), CancellationMode.FreshOnly)
    ).await(99.seconds) ==
      Left(UnknownOrderProblem(OrderId("UNKNOWN"))))
  }

  "Cancel multiple orders with Batch" in {
    val orders = for (i <- 1 to 3) yield
      FreshOrder(
        OrderId(i.toString),
        singleJobWorkflow.path, scheduledFor = Some(Timestamp.now + 99.seconds))
    for (o <- orders) controller.addOrderBlocking(o)
    for (o <- orders) eventWatch.await[OrderAttached](_.key == o.id)
    val response = controllerApi.executeCommand(
      CancelOrders(orders.map(_.id), CancellationMode.FreshOnly)
    ).await(99.seconds).orThrow
    assert(response == Response.Accepted)
    for (o <- orders) eventWatch.await[OrderCancelled](_.key == o.id)
  }
}

object CancelOrdersTest
{
  private val sleepingExecutable = ShellScriptExecutable(
     """#!/usr/bin/env bash
       |set -euo pipefail
       |echo "READY"
       |i=0
       |while [ $i -lt $SLEEP ]; do
       |  i=$(($i + 1))
       |  for j in {0..9}; do
       |    sleep 0.1
       |  done
       |done
       |""".stripMargin,
    Map("SLEEP" -> NamedValue("sleep")))
  private val agentPath = AgentPath("AGENT")
  private val versionId = VersionId("INITIAL")

  private val singleJobWorkflow = Workflow.of(
    WorkflowPath("SINGLE") ~ versionId,
    Execute(WorkflowJob(agentPath, sleepingExecutable)))

  private val trappingExecutable = ShellScriptExecutable(
    """#!/usr/bin/env bash
      |set -euo pipefail
      |trap "" SIGTERM
      |echo "READY"
      |for i in {0..99}; do
      |  sleep 0.1
      |done
      |""".stripMargin)

  private val sigkillDelay = 1.s
  private val sigkillDelayWorkflow = Workflow.of(
    WorkflowPath("SIGKILL-DELAY") ~ versionId,
    Execute(WorkflowJob(agentPath, trappingExecutable, sigkillDelay = Some(sigkillDelay))))

  private val sigkillImmediatelyWorkflow = Workflow.of(
    WorkflowPath("SIGKILL-DELAY-0") ~ versionId,
    Execute(WorkflowJob(agentPath, trappingExecutable, sigkillDelay = Some(0.s))))

  private val twoJobsWorkflow = Workflow.of(
    WorkflowPath("TWO") ~ versionId,
    Execute(WorkflowJob(agentPath, sleepingExecutable)),
    Execute(WorkflowJob(agentPath, sleepingExecutable)))

  private val forkJoinIfFailedWorkflow = Workflow.of(
    WorkflowPath("FORK-JOIN-IF-FAILED") ~ versionId,
    Fork(
      Vector(
        "🥕" -> Workflow.of(
          Execute(WorkflowJob(agentPath, sleepingExecutable)))),
      joinIfFailed = true),
    Execute(WorkflowJob(agentPath, sleepingExecutable)))

  private val forkWorkflow = Workflow.of(
    WorkflowPath("FORK") ~ versionId,
    Fork.of(
      "🥕" -> Workflow.of(
        Execute(WorkflowJob(agentPath, sleepingExecutable)))),
    Execute(WorkflowJob(agentPath, sleepingExecutable)))
}
