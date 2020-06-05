package js7.tests

import js7.base.problem.Checked.Ops
import js7.base.time.ScalaTime._
import js7.base.time.Timestamp
import js7.common.scalautil.MonixUtils.syntax._
import js7.data.Problems.{CancelStartedOrderProblem, UnknownOrderProblem}
import js7.data.agent.AgentRefPath
import js7.data.command.CancelMode
import js7.data.job.ExecutablePath
import js7.data.order.OrderEvent.{OrderAdded, OrderAttachable, OrderCancellationMarked, OrderCancelled, OrderDetachable, OrderFinished, OrderMoved, OrderProcessed, OrderProcessingStarted, OrderStarted, OrderStdWritten, OrderTransferredToAgent, OrderTransferredToMaster}
import js7.data.order.{FreshOrder, OrderEvent, OrderId, Outcome}
import js7.data.workflow.instructions.Execute
import js7.data.workflow.instructions.executable.WorkflowJob
import js7.data.workflow.position.Position
import js7.data.workflow.test.ForkTestSetting.TestExecutablePath
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.master.data.MasterCommand.{Batch, CancelOrder, Response}
import js7.tests.CancelOrderTest._
import js7.tests.testenv.DirectoryProvider.script
import js7.tests.testenv.MasterAgentForScalaTest
import monix.execution.Scheduler.Implicits.global
import org.scalatest.freespec.AnyFreeSpec
import scala.concurrent.duration._

/**
  * @author Joacim Zschimmer
  */
final class CancelOrderTest extends AnyFreeSpec with MasterAgentForScalaTest
{
  protected val agentRefPaths = TestAgentRefPath :: Nil
  protected val fileBased = SingleJobWorkflow :: TwoJobsWorkflow :: Nil

  override def beforeAll() = {
    for (a <- directoryProvider.agents) a.writeExecutable(TestExecutablePath, script(2.s))
    super.beforeAll()
  }

  "Cancel a fresh order" in {
    val order = FreshOrder(OrderId("🔹"), SingleJobWorkflow.id.path, scheduledFor = Some(Timestamp.now + 99.seconds))
    master.addOrderBlocking(order)
    master.eventWatch.await[OrderTransferredToAgent](_.key == order.id)
    master.executeCommandAsSystemUser(CancelOrder(order.id, CancelMode.NotStarted)).await(99.seconds).orThrow
    master.eventWatch.await[OrderCancelled](_.key == order.id)
    assert(master.eventWatch.keyedEvents[OrderEvent](order.id) == Vector(
      OrderAdded(SingleJobWorkflow.id, order.scheduledFor),
      OrderAttachable(TestAgentRefPath),
      OrderTransferredToAgent(TestAgentRefPath),
      OrderCancellationMarked(CancelMode.NotStarted),
      OrderDetachable,
      OrderTransferredToMaster,
      OrderCancelled))
  }

  "Cancel a finishing order" in {
    val order = FreshOrder(OrderId("🔺"), SingleJobWorkflow.id.path)
    master.addOrderBlocking(order)
    master.eventWatch.await[OrderProcessingStarted](_.key == order.id)
    master.executeCommandAsSystemUser(CancelOrder(order.id, CancelMode.FreshOrStarted)).await(99.seconds).orThrow
    master.eventWatch.await[OrderFinished](_.key == order.id)
    assert(master.eventWatch.keyedEvents[OrderEvent](order.id).filterNot(_.isInstanceOf[OrderStdWritten]) == Vector(
      OrderAdded(SingleJobWorkflow.id, order.scheduledFor),
      OrderAttachable(TestAgentRefPath),
      OrderTransferredToAgent(TestAgentRefPath),
      OrderStarted,
      OrderProcessingStarted,
      OrderCancellationMarked(CancelMode.FreshOrStarted),
      OrderProcessed(Outcome.succeeded),
      OrderMoved(Position(1)),
      OrderDetachable,
      OrderTransferredToMaster,
      OrderFinished))
  }

  "Cancelling (mode=NotStarted) a started order is not possible" in {
    val order = FreshOrder(OrderId("❌"), TwoJobsWorkflow.id.path)
    master.addOrderBlocking(order)
    master.eventWatch.await[OrderProcessingStarted](_.key == order.id)
    sleep(100.ms)  // MasterOrderKeeper may take some time to update its state
    // Master knows, the order has started
    assert(master.executeCommandAsSystemUser(CancelOrder(order.id, CancelMode.NotStarted)).await(99.seconds) ==
      Left(CancelStartedOrderProblem(OrderId("❌"))))
  }

  "Cancel a started order between two jobs" in {
    val order = FreshOrder(OrderId("🔴"), TwoJobsWorkflow.id.path)
    master.addOrderBlocking(order)
    master.eventWatch.await[OrderProcessingStarted](_.key == order.id)
    master.executeCommandAsSystemUser(CancelOrder(order.id, CancelMode.FreshOrStarted)).await(99.seconds).orThrow
    master.eventWatch.await[OrderCancelled](_.key == order.id)
    assert(master.eventWatch.keyedEvents[OrderEvent](order.id).filterNot(_.isInstanceOf[OrderStdWritten]) == Vector(
      OrderAdded(TwoJobsWorkflow.id, order.scheduledFor),
      OrderAttachable(TestAgentRefPath),
      OrderTransferredToAgent(TestAgentRefPath),
      OrderStarted,
      OrderProcessingStarted,
      OrderCancellationMarked(CancelMode.FreshOrStarted),
      OrderProcessed(Outcome.succeeded),
      OrderMoved(Position(1)),
      OrderDetachable,
      OrderTransferredToMaster,
      OrderCancelled))
  }

  "Cancel unknown order" in {
    assert(master.executeCommandAsSystemUser(CancelOrder(OrderId("UNKNOWN"), CancelMode.NotStarted)).await(99.seconds) ==
      Left(UnknownOrderProblem(OrderId("UNKNOWN"))))
  }

  "Cancel multiple orders with Batch" in {
    val orders = for (i <- 1 to 3) yield FreshOrder(OrderId(i.toString), SingleJobWorkflow.id.path, scheduledFor = Some(Timestamp.now + 99.seconds))
    for (o <- orders) master.addOrderBlocking(o)
    for (o <- orders) master.eventWatch.await[OrderTransferredToAgent](_.key == o.id)
    val response = master.executeCommandAsSystemUser(Batch(for (o <- orders) yield CancelOrder(o.id, CancelMode.NotStarted))).await(99.seconds).orThrow
    assert(response == Batch.Response(Vector.fill(orders.length)(Right(Response.Accepted))))
    for (o <- orders) master.eventWatch.await[OrderCancelled](_.key == o.id)
  }
}

object CancelOrderTest {
  private val TestExecutablePath = ExecutablePath("/executable.cmd")
  private val TestAgentRefPath = AgentRefPath("/AGENT")
  private val SingleJobWorkflow = Workflow.of(
    WorkflowPath("/SINGLE") ~ "INITIAL",
    Execute(WorkflowJob(TestAgentRefPath, TestExecutablePath)))
  private val TwoJobsWorkflow = Workflow.of(
    WorkflowPath("/TWO") ~ "INITIAL",
    Execute(WorkflowJob(TestAgentRefPath, TestExecutablePath)),
    Execute(WorkflowJob(TestAgentRefPath, TestExecutablePath)))
}
