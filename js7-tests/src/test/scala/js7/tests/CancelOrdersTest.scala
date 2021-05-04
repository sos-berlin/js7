package js7.tests

import js7.base.io.process.ProcessSignal.{SIGKILL, SIGTERM}
import js7.base.io.process.ReturnCode
import js7.base.problem.Checked.Ops
import js7.base.system.OperatingSystem.isWindows
import js7.base.thread.MonixBlocking.syntax._
import js7.base.time.ScalaTime._
import js7.base.time.Timestamp
import js7.data.Problems.{CancelStartedOrderProblem, UnknownOrderProblem}
import js7.data.agent.AgentPath
import js7.data.command.CancelMode
import js7.data.controller.ControllerCommand.{CancelOrders, Response}
import js7.data.item.VersionId
import js7.data.job.RelativePathExecutable
import js7.data.order.OrderEvent.{OrderAdded, OrderAttachable, OrderAttached, OrderCancelMarked, OrderCancelMarkedOnAgent, OrderCancelled, OrderDetachable, OrderDetached, OrderFinished, OrderForked, OrderJoined, OrderMoved, OrderProcessed, OrderProcessingKilled, OrderProcessingStarted, OrderStarted, OrderStdWritten, OrderTerminated}
import js7.data.order.{FreshOrder, OrderEvent, OrderId, Outcome}
import js7.data.value.NamedValues
import js7.data.value.Value.convenience._
import js7.data.value.expression.Expression.NamedValue
import js7.data.workflow.instructions.executable.WorkflowJob
import js7.data.workflow.instructions.{Execute, Fork}
import js7.data.workflow.position.{Position, WorkflowPosition}
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.tests.CancelOrdersTest._
import js7.tests.testenv.ControllerAgentForScalaTest
import js7.tests.testenv.DirectoryProvider.sleepingScript
import monix.execution.Scheduler.Implicits.global
import org.scalatest.freespec.AnyFreeSpec
import scala.concurrent.duration._

/**
  * @author Joacim Zschimmer
  */
final class CancelOrdersTest extends AnyFreeSpec with ControllerAgentForScalaTest
{
  protected val agentPaths = agentPath :: Nil
  protected val versionedItems = singleJobWorkflow :: twoJobsWorkflow :: forkWorkflow :: Nil

  override def beforeAll() = {
    for (a <- directoryProvider.agents) a.writeExecutable(pathExecutable, sleepingScript("SLEEP"))
    super.beforeAll()
  }

  "Cancel a fresh order" in {
    val order = FreshOrder(OrderId("🔹"), singleJobWorkflow.id.path, scheduledFor = Some(Timestamp.now + 99.seconds))
    controller.addOrderBlocking(order)
    controller.eventWatch.await[OrderAttached](_.key == order.id)
    controller.executeCommandAsSystemUser(CancelOrders(Set(order.id), CancelMode.FreshOnly)).await(99.seconds).orThrow
    controller.eventWatch.await[OrderCancelled](_.key == order.id)
    assert(controller.eventWatch.keyedEvents[OrderEvent](order.id) == Vector(
      OrderAdded(singleJobWorkflow.id, order.arguments, order.scheduledFor),
      OrderAttachable(agentPath),
      OrderAttached(agentPath),
      OrderCancelMarked(CancelMode.FreshOnly),
      OrderDetachable,
      OrderDetached,
      OrderCancelled))
  }

  "Cancel a finishing order" in {
    val order = FreshOrder(OrderId("🔺"), singleJobWorkflow.id.path, Map("sleep" -> 1))
    controller.addOrderBlocking(order)
    controller.eventWatch.await[OrderProcessingStarted](_.key == order.id)
    controller.executeCommandAsSystemUser(CancelOrders(Set(order.id), CancelMode.FreshOrStarted())).await(99.seconds).orThrow
    controller.eventWatch.await[OrderFinished](_.key == order.id)
    assert(controller.eventWatch.keyedEvents[OrderEvent](order.id).filterNot(_.isInstanceOf[OrderStdWritten]) == Vector(
      OrderAdded(singleJobWorkflow.id, order.arguments, order.scheduledFor),
      OrderAttachable(agentPath),
      OrderAttached(agentPath),
      OrderStarted,
      OrderProcessingStarted,
      OrderCancelMarked(CancelMode.FreshOrStarted()),
      OrderCancelMarkedOnAgent,
      OrderProcessed(Outcome.succeededRC0),
      OrderMoved(Position(1)),
      OrderDetachable,
      OrderDetached,
      OrderFinished))
  }

  "Cancelling (mode=FreshOnly) a started order is not possible" in {
    val order = FreshOrder(OrderId("❌"), twoJobsWorkflow.id.path, Map("sleep" -> 5))
    controller.addOrderBlocking(order)
    controller.eventWatch.await[OrderProcessingStarted](_.key == order.id)
    sleep(100.ms)  // ControllerOrderKeeper may take some time to update its state
    // Controller knows, the order has started
    assert(controller.executeCommandAsSystemUser(CancelOrders(Set(order.id), CancelMode.FreshOnly)).await(99.seconds) ==
      Left(CancelStartedOrderProblem(OrderId("❌"))))
    controller.executeCommandAsSystemUser(CancelOrders(Set(order.id), CancelMode.FreshOrStarted(Some(CancelMode.Kill()))))
      .await(99.seconds).orThrow
    controller.eventWatch.await[OrderTerminated](_.key == order.id)
  }

  "Cancel a started order between two jobs" in {
    val order = FreshOrder(OrderId("🔴"), twoJobsWorkflow.id.path, Map("sleep" -> 2))
    controller.addOrderBlocking(order)
    controller.eventWatch.await[OrderProcessingStarted](_.key == order.id)
    controller.executeCommandAsSystemUser(CancelOrders(Set(order.id), CancelMode.FreshOrStarted())).await(99.seconds).orThrow
    controller.eventWatch.await[OrderCancelled](_.key == order.id)
    assert(controller.eventWatch.keyedEvents[OrderEvent](order.id).filterNot(_.isInstanceOf[OrderStdWritten]) == Vector(
      OrderAdded(twoJobsWorkflow.id, order.arguments, order.scheduledFor),
      OrderAttachable(agentPath),
      OrderAttached(agentPath),
      OrderStarted,
      OrderProcessingStarted,
      OrderCancelMarked(CancelMode.FreshOrStarted()),
      OrderCancelMarkedOnAgent,
      OrderProcessed(Outcome.succeededRC0),
      OrderMoved(Position(1)),
      OrderDetachable,
      OrderDetached,
      OrderCancelled))
  }

  "Cancel an order and the first job" in {
    val order = FreshOrder(OrderId("⭕️"), singleJobWorkflow.id.path, Map("sleep" -> 9))
    testCancelFirstJob(order, Some(singleJobWorkflow.id /: Position(0)), immediately = false)
  }

  "Cancel an order but not the first job" in {
    val order = FreshOrder(OrderId("🔶"), twoJobsWorkflow.id.path, Map("sleep" -> 2))
    testCancel(order, Some(twoJobsWorkflow.id /: Position(1)), immediately = false,
      mode => Vector(
        OrderAdded(twoJobsWorkflow.id, order.arguments),
        OrderAttachable(agentPath),
        OrderAttached(agentPath),
        OrderStarted,
        OrderProcessingStarted,
        OrderCancelMarked(mode),
        OrderCancelMarkedOnAgent,
        OrderProcessed(Outcome.succeededRC0),
        OrderMoved(Position(1)),
        OrderDetachable,
        OrderDetached,
        OrderCancelled))
  }

  "Cancel an order and the currently running job" in {
    val order = FreshOrder(OrderId("🔷"), singleJobWorkflow.id.path, Map("sleep" -> 9))
    testCancelFirstJob(order, None, immediately = false)
  }

  "Cancel an order and a certain running job with SIGKILL" in {
    val order = FreshOrder(OrderId("🔵"), singleJobWorkflow.id.path, Map("sleep" -> 9))
    testCancelFirstJob(order, Some(singleJobWorkflow.id /: Position(0)),
      immediately = true)
  }

  private def testCancelFirstJob(order: FreshOrder, workflowPosition: Option[WorkflowPosition], immediately: Boolean): Unit =
    testCancel(order, workflowPosition, immediately = immediately,
      mode => Vector(
        OrderAdded(order.workflowPath ~ versionId, order.arguments),
        OrderAttachable(agentPath),
        OrderAttached(agentPath),
        OrderStarted,
        OrderProcessingStarted,
        OrderCancelMarked(mode),
        OrderCancelMarkedOnAgent,
        OrderProcessed(Outcome.Killed(Outcome.Failed(NamedValues.rc(
          if (isWindows) ReturnCode(0) else ReturnCode(if (immediately) SIGKILL else SIGTERM))))),
        OrderProcessingKilled,
        OrderDetachable,
        OrderDetached,
        OrderCancelled))

  "Cancel a forked order and kill job" in {
    val order = FreshOrder(OrderId("FORK"), forkWorkflow.id.path, Map("sleep" -> 2))
    controller.addOrderBlocking(order)
    controller.eventWatch.await[OrderProcessingStarted](_.key == (order.id | "🥕"))
    val mode = CancelMode.FreshOrStarted(Some(CancelMode.Kill()))
    controller.executeCommandAsSystemUser(CancelOrders(Set(order.id), mode))
      .await(99.seconds).orThrow
    controller.eventWatch.await[OrderCancelled](_.key == order.id)
    assert(controller.eventWatch
      .keyedEvents[OrderEvent]
      .filter(_.key.string startsWith "FORK")
      .filterNot(_.event.isInstanceOf[OrderStdWritten]) ==
      Vector(
        OrderId("FORK") <-: OrderAdded(forkWorkflow.id, order.arguments, order.scheduledFor),
        OrderId("FORK") <-: OrderStarted,
        OrderId("FORK") <-: OrderForked(Seq(OrderForked.Child(Fork.Branch.Id("🥕"), OrderId("FORK|🥕")))),
        OrderId("FORK|🥕") <-: OrderAttachable(agentPath),
        OrderId("FORK|🥕") <-: OrderAttached(agentPath),
        OrderId("FORK|🥕") <-: OrderProcessingStarted,
        OrderId("FORK") <-: OrderCancelMarked(mode),
        OrderId("FORK|🥕") <-: OrderProcessed(Outcome.succeededRC0),
        OrderId("FORK|🥕") <-: OrderMoved(Position(0) / "fork+🥕" % 1),
        OrderId("FORK|🥕") <-: OrderDetachable,
        OrderId("FORK|🥕") <-: OrderDetached,
        OrderId("FORK") <-: OrderJoined(Outcome.succeeded),
        OrderId("FORK") <-: OrderMoved(Position(1)),
        OrderId("FORK") <-: OrderCancelled))
  }

  "Cancel with sigkillDelay" in {
    pending  // TODO
  }

  private def testCancel(order: FreshOrder, workflowPosition: Option[WorkflowPosition], immediately: Boolean,
    expectedEvents: CancelMode => Seq[OrderEvent])
  : Unit = {
    controller.addOrderBlocking(order)
    controller.eventWatch.await[OrderProcessingStarted](_.key == order.id)
    val mode = CancelMode.FreshOrStarted(Some(CancelMode.Kill(immediately = immediately, workflowPosition)))
    controller.executeCommandAsSystemUser(CancelOrders(Set(order.id), mode))
      .await(99.seconds).orThrow
    controller.eventWatch.await[OrderCancelled](_.key == order.id)
    assert(controller.eventWatch.keyedEvents[OrderEvent](order.id).filterNot(_.isInstanceOf[OrderStdWritten]) ==
      expectedEvents(mode))
  }

  "Cancel unknown order" in {
    assert(controller.executeCommandAsSystemUser(CancelOrders(Set(OrderId("UNKNOWN")), CancelMode.FreshOnly)).await(99.seconds) ==
      Left(UnknownOrderProblem(OrderId("UNKNOWN"))))
  }

  "Cancel multiple orders with Batch" in {
    val orders = for (i <- 1 to 3) yield FreshOrder(OrderId(i.toString), singleJobWorkflow.id.path, scheduledFor = Some(Timestamp.now + 99.seconds))
    for (o <- orders) controller.addOrderBlocking(o)
    for (o <- orders) controller.eventWatch.await[OrderAttached](_.key == o.id)
    val response = controller.executeCommandAsSystemUser(CancelOrders(orders.map(_.id), CancelMode.FreshOnly)).await(99.seconds).orThrow
    assert(response == Response.Accepted)
    for (o <- orders) controller.eventWatch.await[OrderCancelled](_.key == o.id)
  }
}

object CancelOrdersTest
{
  private val pathExecutable = RelativePathExecutable("executable.cmd",
    Map("SLEEP" -> NamedValue.last("sleep")))
  private val agentPath = AgentPath("AGENT")
  private val versionId = VersionId("INITIAL")

  private val singleJobWorkflow = Workflow.of(
    WorkflowPath("SINGLE") ~ versionId,
    Execute(WorkflowJob(agentPath, pathExecutable)))

  private val twoJobsWorkflow = Workflow.of(
    WorkflowPath("TWO") ~ versionId,
    Execute(WorkflowJob(agentPath, pathExecutable)),
    Execute(WorkflowJob(agentPath, pathExecutable)))

  private val forkWorkflow = Workflow.of(
    WorkflowPath("FORK") ~ versionId,
    Fork.of(
      "🥕" -> Workflow.of(
        Execute(WorkflowJob(agentPath, pathExecutable)))),
    Execute(WorkflowJob(agentPath, pathExecutable)))
}
