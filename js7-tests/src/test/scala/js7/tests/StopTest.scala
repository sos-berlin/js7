package js7.tests

import js7.base.configutils.Configs.HoconStringInterpolator
import js7.base.problem.Checked.Ops
import js7.base.test.OurTestSuite
import js7.base.thread.CatsBlocking.syntax.*
import js7.base.time.ScalaTime.DurationRichInt
import js7.data.agent.AgentPath
import js7.data.controller.ControllerCommand.ResumeOrder
import js7.data.order.Order.Stopped
import js7.data.order.OrderEvent.{OrderAdded, OrderAttachable, OrderAttached, OrderDetachable, OrderDetached, OrderFinished, OrderMoved, OrderProcessed, OrderProcessingStarted, OrderResumed, OrderStarted, OrderStopped, OrderTerminated}
import js7.data.order.{FreshOrder, OrderEvent, OrderId, OrderOutcome}
import js7.data.workflow.instructions.Stop
import js7.data.workflow.position.Position
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.tests.StopTest.*
import js7.tests.jobs.EmptyJob
import js7.tests.testenv.DirectoryProvider.toLocalSubagentId
import js7.tests.testenv.{BlockingItemUpdater, ControllerAgentForScalaTest}
import cats.effect.unsafe.IORuntime

final class StopTest extends OurTestSuite, ControllerAgentForScalaTest, BlockingItemUpdater:

  override protected val controllerConfig = config"""
    js7.auth.users.TEST-USER.permissions = [ UpdateItem ]
    js7.controller.agent-driver.command-batch-delay = 0ms
    js7.controller.agent-driver.event-buffer-delay = 0ms
  """

  override protected val agentConfig = config"""
    js7.job.execution.signed-script-injection-allowed = on
    """

  protected val agentPaths = Seq(agentPath)
  protected val items = Nil

  "Stop at Controller" in:
    val workflow = Workflow.of(WorkflowPath("CONTROLLER-WORKFLOW"),
      Stop())
    withTemporaryItem(workflow) { workflow =>
      val orderId = OrderId("STOP-AT-CONTROLLER")
      controller.api.addOrder(FreshOrder(orderId, workflow.path)).await(99.s).orThrow
      eventWatch.await[OrderStopped](_.key == orderId)

      val order = controllerState.idToOrder(orderId)
      assert(order.isState[Stopped] && order.isStarted && !order.isFailed)

      controller.api
        .executeCommand(
          ResumeOrder(orderId, position = Some(order.position.increment)))
        .await(99.s).orThrow
      eventWatch.await[OrderTerminated](_.key == orderId)

      assert(eventWatch.eventsByKey[OrderEvent](orderId) == Seq(
        OrderAdded(workflow.id),
        OrderStarted,
        OrderStopped,
        OrderResumed(Some(Position(1))),
        OrderFinished()))
    }

  "Stop at Agent" in:
    val workflow = Workflow.of(WorkflowPath("AGENT-WORKFLOW"),
      EmptyJob.execute(agentPath),
      Stop())
    withTemporaryItem(workflow) { workflow =>
      val orderId = OrderId("STOP-AT-AGENT")
      controller.api.addOrder(FreshOrder(orderId, workflow.path)).await(99.s).orThrow
      eventWatch.await[OrderStopped](_.key == orderId)

      val order = controllerState.idToOrder(orderId)
      assert(order.isState[Stopped] && order.isDetached)

      controller.api
        .executeCommand(
          ResumeOrder(orderId, position = Some(order.position.increment)))
        .await(99.s).orThrow
      eventWatch.await[OrderTerminated](_.key == orderId)

      assert(eventWatch.eventsByKey[OrderEvent](orderId) == Seq(
        OrderAdded(workflow.id),
        OrderAttachable(agentPath),
        OrderAttached(agentPath),
        OrderStarted,
        OrderProcessingStarted(Some(subagentId)),
        OrderProcessed(OrderOutcome.succeeded),
        OrderMoved(Position(1)),
        OrderDetachable,
        OrderDetached,
        OrderStopped,
        OrderResumed(Some(Position(2))),
        OrderFinished()))
    }


object StopTest:
  private val agentPath = AgentPath("AGENT")
  private val subagentId = toLocalSubagentId(agentPath)
