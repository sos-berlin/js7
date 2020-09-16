package js7.tests

import js7.base.problem.Checked.Ops
import js7.base.process.ProcessSignal
import js7.base.process.ProcessSignal.{SIGKILL, SIGTERM}
import js7.base.time.ScalaTime._
import js7.base.time.Timestamp
import js7.common.scalautil.MonixUtils.syntax._
import js7.common.system.OperatingSystem.isWindows
import js7.controller.data.ControllerCommand.{Batch, CancelOrder, Response}
import js7.data.Problems.{CancelStartedOrderProblem, UnknownOrderProblem}
import js7.data.agent.AgentRefPath
import js7.data.command.CancelMode
import js7.data.item.VersionId
import js7.data.job.{ExecutablePath, ReturnCode}
import js7.data.order.OrderEvent.{OrderAdded, OrderAttachable, OrderAttached, OrderCancelMarked, OrderCancelled, OrderDetachable, OrderDetached, OrderFinished, OrderForked, OrderJoined, OrderMoved, OrderProcessed, OrderProcessingCancelled, OrderProcessingStarted, OrderStarted, OrderStdWritten}
import js7.data.order.{FreshOrder, OrderEvent, OrderId, Outcome}
import js7.data.workflow.instructions.executable.WorkflowJob
import js7.data.workflow.instructions.{Execute, Fork}
import js7.data.workflow.position.{Position, WorkflowPosition}
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.tests.CancelOrderTest._
import js7.tests.testenv.ControllerAgentForScalaTest
import js7.tests.testenv.DirectoryProvider.script
import monix.execution.Scheduler.Implicits.global
import org.scalatest.freespec.AnyFreeSpec
import scala.concurrent.duration._

/**
  * @author Joacim Zschimmer
  */
final class CancelOrderTest extends AnyFreeSpec with ControllerAgentForScalaTest
{
  protected val agentRefPaths = agentRefPath :: Nil
  protected val inventoryItems = singleJobWorkflow :: twoJobsWorkflow :: forkWorkflow :: Nil

  override def beforeAll() = {
    for (a <- directoryProvider.agents) a.writeExecutable(executablePath, script(2.s))
    super.beforeAll()
  }

  "Cancel a fresh order" in {
    val order = FreshOrder(OrderId("🔹"), singleJobWorkflow.id.path, scheduledFor = Some(Timestamp.now + 99.seconds))
    controller.addOrderBlocking(order)
    controller.eventWatch.await[OrderAttached](_.key == order.id)
    controller.executeCommandAsSystemUser(CancelOrder(order.id, CancelMode.NotStarted)).await(99.seconds).orThrow
    controller.eventWatch.await[OrderCancelled](_.key == order.id)
    assert(controller.eventWatch.keyedEvents[OrderEvent](order.id) == Vector(
      OrderAdded(singleJobWorkflow.id, order.scheduledFor),
      OrderAttachable(agentRefPath),
      OrderAttached(agentRefPath),
      OrderCancelMarked(CancelMode.NotStarted),
      OrderDetachable,
      OrderDetached,
      OrderCancelled))
  }

  "Cancel a finishing order" in {
    val order = FreshOrder(OrderId("🔺"), singleJobWorkflow.id.path)
    controller.addOrderBlocking(order)
    controller.eventWatch.await[OrderProcessingStarted](_.key == order.id)
    controller.executeCommandAsSystemUser(CancelOrder(order.id, CancelMode.FreshOrStarted())).await(99.seconds).orThrow
    controller.eventWatch.await[OrderFinished](_.key == order.id)
    assert(controller.eventWatch.keyedEvents[OrderEvent](order.id).filterNot(_.isInstanceOf[OrderStdWritten]) == Vector(
      OrderAdded(singleJobWorkflow.id, order.scheduledFor),
      OrderAttachable(agentRefPath),
      OrderAttached(agentRefPath),
      OrderStarted,
      OrderProcessingStarted,
      OrderCancelMarked(CancelMode.FreshOrStarted()),
      OrderProcessed(Outcome.succeeded),
      OrderMoved(Position(1)),
      OrderDetachable,
      OrderDetached,
      OrderFinished))
  }

  "Cancelling (mode=NotStarted) a started order is not possible" in {
    val order = FreshOrder(OrderId("❌"), twoJobsWorkflow.id.path)
    controller.addOrderBlocking(order)
    controller.eventWatch.await[OrderProcessingStarted](_.key == order.id)
    sleep(100.ms)  // ControllerOrderKeeper may take some time to update its state
    // Controller knows, the order has started
    assert(controller.executeCommandAsSystemUser(CancelOrder(order.id, CancelMode.NotStarted)).await(99.seconds) ==
      Left(CancelStartedOrderProblem(OrderId("❌"))))
  }

  "Cancel a started order between two jobs" in {
    val order = FreshOrder(OrderId("🔴"), twoJobsWorkflow.id.path)
    controller.addOrderBlocking(order)
    controller.eventWatch.await[OrderProcessingStarted](_.key == order.id)
    controller.executeCommandAsSystemUser(CancelOrder(order.id, CancelMode.FreshOrStarted())).await(99.seconds).orThrow
    controller.eventWatch.await[OrderCancelled](_.key == order.id)
    assert(controller.eventWatch.keyedEvents[OrderEvent](order.id).filterNot(_.isInstanceOf[OrderStdWritten]) == Vector(
      OrderAdded(twoJobsWorkflow.id, order.scheduledFor),
      OrderAttachable(agentRefPath),
      OrderAttached(agentRefPath),
      OrderStarted,
      OrderProcessingStarted,
      OrderCancelMarked(CancelMode.FreshOrStarted()),
      OrderProcessed(Outcome.succeeded),
      OrderMoved(Position(1)),
      OrderDetachable,
      OrderDetached,
      OrderCancelled))
  }

  "Cancel an order and the first job" in {
    testCancelFirstJob(FreshOrder(OrderId("⭕️"), singleJobWorkflow.id.path), Some(singleJobWorkflow.id /: Position(0)), SIGTERM)
  }

  "Cancel an order but not the first job" in {
    testCancel(FreshOrder(OrderId("🔶"), twoJobsWorkflow.id.path), Some(twoJobsWorkflow.id /: Position(1)), SIGTERM,
      mode => Vector(
        OrderAdded(twoJobsWorkflow.id, None),
        OrderAttachable(agentRefPath),
        OrderAttached(agentRefPath),
        OrderStarted,
        OrderProcessingStarted,
        OrderCancelMarked(mode),
        OrderProcessed(Outcome.succeeded),
        OrderMoved(Position(1)),
        OrderDetachable,
        OrderDetached,
        OrderCancelled))
  }

  "Cancel an order and the currently running job" in {
    testCancelFirstJob(FreshOrder(OrderId("🔷"), singleJobWorkflow.id.path), None, SIGTERM)
  }

  "Cancel an order and a certain running job with SIGKILL" in {
    testCancelFirstJob(FreshOrder(OrderId("🔵"), singleJobWorkflow.id.path), Some(singleJobWorkflow.id /: Position(0)), SIGKILL)
  }

  private def testCancelFirstJob(order: FreshOrder, workflowPosition: Option[WorkflowPosition], signal: ProcessSignal): Unit =
    testCancel(order, workflowPosition, signal,
      mode => Vector(
        OrderAdded(order.workflowPath ~ versionId, None),
        OrderAttachable(agentRefPath),
        OrderAttached(agentRefPath),
        OrderStarted,
        OrderProcessingStarted,
        OrderCancelMarked(mode),
        OrderProcessed(Outcome.Cancelled(Outcome.Failed(if (isWindows) ReturnCode(0) else ReturnCode(signal)))),
        OrderProcessingCancelled,
        OrderDetachable,
        OrderDetached,
        OrderCancelled))

  "Cancel a forked order and kill job" in {
    val order = FreshOrder(OrderId("FORK"), forkWorkflow.id.path)
    controller.addOrderBlocking(order)
    controller.eventWatch.await[OrderProcessingStarted](_.key == order.id / "🥕")
    val mode = CancelMode.FreshOrStarted(Some(CancelMode.Kill(SIGTERM)))
    controller.executeCommandAsSystemUser(CancelOrder(order.id, mode))
      .await(99.seconds).orThrow
    controller.eventWatch.await[OrderCancelled](_.key == order.id)
    assert(controller.eventWatch
      .keyedEvents[OrderEvent]
      .filter(_.key.string startsWith "FORK")
      .filterNot(_.event.isInstanceOf[OrderStdWritten]) ==
      Vector(
        OrderId("FORK") <-: OrderAdded(forkWorkflow.id, order.scheduledFor),
        OrderId("FORK") <-: OrderStarted,
        OrderId("FORK") <-: OrderForked(Seq(OrderForked.Child(Fork.Branch.Id("🥕"), OrderId("FORK/🥕")))),
        OrderId("FORK/🥕") <-: OrderAttachable(agentRefPath),
        OrderId("FORK/🥕") <-: OrderAttached(agentRefPath),
        OrderId("FORK/🥕") <-: OrderProcessingStarted,
        OrderId("FORK") <-: OrderCancelMarked(mode),
        OrderId("FORK/🥕") <-: OrderProcessed(Outcome.succeeded),
        OrderId("FORK/🥕") <-: OrderMoved(Position(0) / "fork+🥕" % 1),
        OrderId("FORK/🥕") <-: OrderDetachable,
        OrderId("FORK/🥕") <-: OrderDetached,
        OrderId("FORK") <-: OrderJoined(Outcome.succeeded),
        OrderId("FORK") <-: OrderMoved(Position(1)),
        OrderId("FORK") <-: OrderCancelled))
  }

  "Cancel with sigkillAfter" in {
    pending  // TODO
  }

  private def testCancel(order: FreshOrder, workflowPosition: Option[WorkflowPosition], signal: ProcessSignal,
    expectedEvents: CancelMode => Seq[OrderEvent])
  : Unit = {
    controller.addOrderBlocking(order)
    controller.eventWatch.await[OrderProcessingStarted](_.key == order.id)
    val mode = CancelMode.FreshOrStarted(Some(CancelMode.Kill(signal, workflowPosition)))
    controller.executeCommandAsSystemUser(CancelOrder(order.id, mode))
      .await(99.seconds).orThrow
    controller.eventWatch.await[OrderCancelled](_.key == order.id)
    assert(controller.eventWatch.keyedEvents[OrderEvent](order.id).filterNot(_.isInstanceOf[OrderStdWritten]) ==
      expectedEvents(mode))
  }

  "Cancel unknown order" in {
    assert(controller.executeCommandAsSystemUser(CancelOrder(OrderId("UNKNOWN"), CancelMode.NotStarted)).await(99.seconds) ==
      Left(UnknownOrderProblem(OrderId("UNKNOWN"))))
  }

  "Cancel multiple orders with Batch" in {
    val orders = for (i <- 1 to 3) yield FreshOrder(OrderId(i.toString), singleJobWorkflow.id.path, scheduledFor = Some(Timestamp.now + 99.seconds))
    for (o <- orders) controller.addOrderBlocking(o)
    for (o <- orders) controller.eventWatch.await[OrderAttached](_.key == o.id)
    val response = controller.executeCommandAsSystemUser(Batch(for (o <- orders) yield CancelOrder(o.id, CancelMode.NotStarted))).await(99.seconds).orThrow
    assert(response == Batch.Response(Vector.fill(orders.length)(Right(Response.Accepted))))
    for (o <- orders) controller.eventWatch.await[OrderCancelled](_.key == o.id)
  }
}

object CancelOrderTest
{
  private val executablePath = ExecutablePath("/executable.cmd")
  private val agentRefPath = AgentRefPath("/AGENT")
  private val versionId = VersionId("INITIAL")

  private val singleJobWorkflow = Workflow.of(
    WorkflowPath("/SINGLE") ~ versionId,
    Execute(WorkflowJob(agentRefPath, executablePath)))

  private val twoJobsWorkflow = Workflow.of(
    WorkflowPath("/TWO") ~ versionId,
    Execute(WorkflowJob(agentRefPath, executablePath)),
    Execute(WorkflowJob(agentRefPath, executablePath)))

  private val forkWorkflow = Workflow.of(
    WorkflowPath("/FORK") ~ versionId,
    Fork.of(
      "🥕" -> Workflow.of(
        Execute(WorkflowJob(agentRefPath, executablePath)))),
    Execute(WorkflowJob(agentRefPath, executablePath)))
}
