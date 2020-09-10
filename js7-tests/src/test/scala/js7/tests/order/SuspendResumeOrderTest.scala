package js7.tests.order

import js7.base.problem.Checked.Ops
import js7.base.problem.Problem
import js7.base.time.ScalaTime._
import js7.base.time.Timestamp
import js7.common.configutils.Configs.HoconStringInterpolator
import js7.common.scalautil.MonixUtils.syntax._
import js7.controller.data.ControllerCommand.{Batch, CancelOrder, Response, ResumeOrder, SuspendOrder}
import js7.data.Problems.UnknownOrderProblem
import js7.data.agent.AgentRefPath
import js7.data.command.CancelMode
import js7.data.item.VersionId
import js7.data.job.ExecutablePath
import js7.data.order.OrderEvent.{OrderAdded, OrderAttachable, OrderDetachable, OrderFinished, OrderForked, OrderJoined, OrderMoved, OrderProcessed, OrderProcessingStarted, OrderResumeMarked, OrderResumed, OrderStarted, OrderStdWritten, OrderStdoutWritten, OrderSuspendMarked, OrderSuspended, OrderTransferredToAgent, OrderTransferredToController}
import js7.data.order.{FreshOrder, OrderEvent, OrderId, Outcome}
import js7.data.workflow.instructions.executable.WorkflowJob
import js7.data.workflow.instructions.{Execute, Fork}
import js7.data.workflow.position.Position
import js7.data.workflow.test.ForkTestSetting.TestExecutablePath
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.tests.order.SuspendResumeOrderTest._
import js7.tests.testenv.ControllerAgentForScalaTest
import js7.tests.testenv.DirectoryProvider.script
import monix.execution.Scheduler.Implicits.global
import org.scalatest.freespec.AnyFreeSpec

final class SuspendResumeOrderTest extends AnyFreeSpec with ControllerAgentForScalaTest
{
  protected val agentRefPaths = agentRefPath :: Nil
  protected val inventoryItems = singleJobWorkflow :: twoJobsWorkflow :: forkWorkflow :: Nil
  override def controllerConfig = config"js7.journal.remove-obsolete-files = false" withFallback super.controllerConfig

  override def beforeAll() = {
    for (a <- directoryProvider.agents) a.writeExecutable(TestExecutablePath, script(1.s))
    super.beforeAll()
  }

  "Suspend and resume a fresh order" in {
    controller.eventWatch.await[AgentReady]()
    val order = FreshOrder(OrderId("🔹"), singleJobWorkflow.id.path, scheduledFor = Some(Timestamp.now + 2.s))
    controller.addOrderBlocking(order)
    controller.eventWatch.await[OrderTransferredToAgent](_.key == order.id)

    controller.executeCommandAsSystemUser(SuspendOrder(order.id)).await(99.s).orThrow
    controller.eventWatch.await[OrderSuspended](_.key == order.id)
    assert(controller.eventWatch.keyedEvents[OrderEvent](order.id) == Seq(
      OrderAdded(singleJobWorkflow.id, order.scheduledFor),
      OrderAttachable(agentRefPath),
      OrderTransferredToAgent(agentRefPath),
      OrderSuspendMarked,
      OrderDetachable,
      OrderTransferredToController,
      OrderSuspended))
    val lastEventId = controller.eventWatch.lastAddedEventId

    controller.executeCommandAsSystemUser(ResumeOrder(order.id)).await(99.s).orThrow

    // ResumeOrder command expected a suspended or suspending order
    assert(controller.executeCommandAsSystemUser(ResumeOrder(order.id)).await(99.s) ==
      Left(Problem("Order cannot resume because it is not suspended")))

    controller.eventWatch.await[OrderFinished](_.key == order.id)
    assert(controller.eventWatch.keyedEvents[OrderEvent](order.id, after = lastEventId) == Seq(
      OrderResumed,
      OrderAttachable(agentRefPath),
      OrderTransferredToAgent(agentRefPath),
      OrderStarted,
      OrderProcessingStarted,
      OrderStdoutWritten("TEST ☘\n"),
      OrderProcessed(Outcome.succeeded),
      OrderMoved(Position(1)),
      OrderDetachable,
      OrderTransferredToController,
      OrderFinished))
  }

  "A finishing order is not suspendible" in {
    val order = FreshOrder(OrderId("🔺"), singleJobWorkflow.id.path)
    controller.addOrderBlocking(order)
    controller.eventWatch.await[OrderProcessingStarted](_.key == order.id)
    controller.executeCommandAsSystemUser(SuspendOrder(order.id)).await(99.s).orThrow
    controller.eventWatch.await[OrderFinished](_.key == order.id)
    assert(controller.eventWatch.keyedEvents[OrderEvent](order.id).filterNot(_.isInstanceOf[OrderStdWritten]) == Seq(
      OrderAdded(singleJobWorkflow.id, order.scheduledFor),
      OrderAttachable(agentRefPath),
      OrderTransferredToAgent(agentRefPath),
      OrderStarted,
      OrderProcessingStarted,
      OrderSuspendMarked,
      OrderProcessed(Outcome.succeeded),
      OrderMoved(Position(1)),
      OrderDetachable,
      OrderTransferredToController,
      OrderFinished))
  }

  "Suspend and resume an order between two jobs" in {
    val order = FreshOrder(OrderId("🔴"), twoJobsWorkflow.id.path)
    controller.addOrderBlocking(order)
    controller.eventWatch.await[OrderProcessingStarted](_.key == order.id)

    controller.executeCommandAsSystemUser(SuspendOrder(order.id)).await(99.s).orThrow
    controller.eventWatch.await[OrderSuspended](_.key == order.id)
    assert(controller.eventWatch.keyedEvents[OrderEvent](order.id).filterNot(_.isInstanceOf[OrderStdWritten]) == Seq(
      OrderAdded(twoJobsWorkflow.id, order.scheduledFor),
      OrderAttachable(agentRefPath),
      OrderTransferredToAgent(agentRefPath),
      OrderStarted,
      OrderProcessingStarted,
      OrderSuspendMarked,
      OrderProcessed(Outcome.succeeded),
      OrderMoved(Position(1)),
      OrderDetachable,
      OrderTransferredToController,
      OrderSuspended))

    val lastEventId = controller.eventWatch.lastAddedEventId
    controller.executeCommandAsSystemUser(ResumeOrder(order.id)).await(99.s).orThrow
    controller.eventWatch.await[OrderFinished](_.key == order.id)
    assert(controller.eventWatch.keyedEvents[OrderEvent](order.id, after = lastEventId)
      .filterNot(_.isInstanceOf[OrderStdWritten]) == Seq(
        OrderResumed,
        OrderAttachable(agentRefPath),
        OrderTransferredToAgent(agentRefPath),
        OrderProcessingStarted,
        OrderProcessed(Outcome.succeeded),
        OrderMoved(Position(2)),
        OrderDetachable,
        OrderTransferredToController,
        OrderFinished))
  }

  "An order being cancelled is not suspendible nor resumable" in {
    val order = FreshOrder(OrderId("🔷"), twoJobsWorkflow.id.path)
    controller.addOrderBlocking(order)
    controller.eventWatch.await[OrderProcessingStarted](_.key == order.id)

    controller.executeCommandAsSystemUser(CancelOrder(order.id, CancelMode.FreshOrStarted())).await(99.s).orThrow
    assert(controller.executeCommandAsSystemUser(SuspendOrder(order.id)).await(99.s) ==
      Left(Problem("Order cannot be suspended because it is cancelled")))
  }

  "Suspend a forked order - child orders are not suspended" in {
    val order = FreshOrder(OrderId("FORK"), forkWorkflow.id.path)
    controller.addOrderBlocking(order)
    controller.eventWatch.await[OrderProcessingStarted](_.key == order.id / "🥕")
    controller.executeCommandAsSystemUser(SuspendOrder(order.id)).await(99.s).orThrow
    controller.eventWatch.await[OrderSuspended](_.key == order.id)
    assert(controller.eventWatch
      .keyedEvents[OrderEvent]
      .filter(_.key.string startsWith "FORK")
      .filterNot(_.event.isInstanceOf[OrderStdWritten]) ==
      Seq(
        OrderId("FORK") <-: OrderAdded(forkWorkflow.id, order.scheduledFor),
        OrderId("FORK") <-: OrderStarted,
        OrderId("FORK") <-: OrderForked(Seq(OrderForked.Child(Fork.Branch.Id("🥕"), OrderId("FORK/🥕")))),
        OrderId("FORK/🥕") <-: OrderAttachable(agentRefPath),
        OrderId("FORK/🥕") <-: OrderTransferredToAgent(agentRefPath),
        OrderId("FORK/🥕") <-: OrderProcessingStarted,
        OrderId("FORK") <-: OrderSuspendMarked,
        OrderId("FORK/🥕") <-: OrderProcessed(Outcome.succeeded),
        OrderId("FORK/🥕") <-: OrderMoved(Position(0) / "fork+🥕" % 1),
        OrderId("FORK/🥕") <-: OrderProcessingStarted,
        OrderId("FORK/🥕") <-: OrderProcessed(Outcome.succeeded),
        OrderId("FORK/🥕") <-: OrderMoved(Position(0) / "fork+🥕" % 2),
        OrderId("FORK/🥕") <-: OrderDetachable,
        OrderId("FORK/🥕") <-: OrderTransferredToController,
        OrderId("FORK") <-: OrderJoined(Outcome.succeeded),
        OrderId("FORK") <-: OrderMoved(Position(1)),
        OrderId("FORK") <-: OrderSuspended))
  }

  "Suspend unknown order" in {
    assert(controller.executeCommandAsSystemUser(SuspendOrder(OrderId("UNKNOWN"))).await(99.s) ==
      Left(UnknownOrderProblem(OrderId("UNKNOWN"))))
  }

  "Suspend multiple orders with Batch" in {
    val orders = for (i <- 1 to 3) yield
      FreshOrder(OrderId(i.toString), singleJobWorkflow.id.path, scheduledFor = Some(Timestamp.now + 99.s))
    for (o <- orders) controller.addOrderBlocking(o)
    for (o <- orders) controller.eventWatch.await[OrderTransferredToAgent](_.key == o.id)
    val response = controller.executeCommandAsSystemUser(Batch(for (o <- orders) yield SuspendOrder(o.id))).await(99.s).orThrow
    assert(response == Batch.Response(Vector.fill(orders.length)(Right(Response.Accepted))))
    for (o <- orders) controller.eventWatch.await[OrderSuspended](_.key == o.id)
  }

  "Resume a still suspending order" in {
    val order = FreshOrder(OrderId("🔵"), twoJobsWorkflow.id.path)
    controller.addOrderBlocking(order)
    controller.eventWatch.await[OrderProcessingStarted](_.key == order.id)

    controller.executeCommandAsSystemUser(SuspendOrder(order.id)).await(99.s).orThrow
    controller.executeCommandAsSystemUser(ResumeOrder(order.id)).await(99.s).orThrow
    controller.eventWatch.await[OrderFinished](_.key == order.id)

    assert(controller.eventWatch.keyedEvents[OrderEvent](order.id).filterNot(_.isInstanceOf[OrderStdWritten]) == Seq(
      OrderAdded(twoJobsWorkflow.id, order.scheduledFor),
      OrderAttachable(agentRefPath),
      OrderTransferredToAgent(agentRefPath),
      OrderStarted,
      OrderProcessingStarted,
      OrderSuspendMarked,
      OrderResumeMarked,
      OrderProcessed(Outcome.succeeded),
      OrderMoved(Position(1)),

      // AgentOrderKeeper does not properly handle simulataneous ExecuteMarkOrder commands
      // and so order is detached for suspending (which has been withdrawn by ResumeOrder).
      OrderDetachable,
      OrderTransferredToController,
      OrderAttachable(agentRefPath),
      OrderTransferredToAgent(agentRefPath),

      OrderProcessingStarted,
      OrderProcessed(Outcome.succeeded),
      OrderMoved(Position(2)),
      OrderDetachable,
      OrderTransferredToController,
      OrderFinished))
  }
}

object SuspendResumeOrderTest
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
        Execute(WorkflowJob(agentRefPath, executablePath)),
        Execute(WorkflowJob(agentRefPath, executablePath)))),
    Execute(WorkflowJob(agentRefPath, executablePath)))
}
