package js7.tests

import js7.base.configutils.Configs.HoconStringInterpolator
import js7.base.problem.Checked.Ops
import js7.base.problem.Problem
import js7.base.test.OurTestSuite
import js7.base.thread.MonixBlocking.syntax.RichTask
import js7.base.time.ScalaTime.DurationRichInt
import js7.data.agent.AgentPath
import js7.data.controller.ControllerCommand.ResumeOrder
import js7.data.order.Order.{Stopped, StoppedWhileFresh}
import js7.data.order.OrderEvent.{OrderAdded, OrderAttachable, OrderAttached, OrderDetachable, OrderDetached, OrderFailed, OrderFinished, OrderMoved, OrderOutcomeAdded, OrderProcessed, OrderProcessingStarted, OrderResumed, OrderStarted, OrderStopped, OrderTerminated}
import js7.data.order.{FreshOrder, HistoricOutcome, OrderEvent, OrderId, Outcome}
import js7.data.value.BooleanValue
import js7.data.value.expression.ExpressionParser.expr
import js7.data.workflow.instructions.{Fail, If, Options}
import js7.data.workflow.position.Position
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.tests.StopOnFailureTest.*
import js7.tests.jobs.FailingJob
import js7.tests.testenv.DirectoryProvider.toLocalSubagentId
import js7.tests.testenv.{BlockingItemUpdater, ControllerAgentForScalaTest}
import monix.execution.Scheduler.Implicits.traced

final class StopOnFailureTest extends OurTestSuite with ControllerAgentForScalaTest
with BlockingItemUpdater
{
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

  "Fail at Controller" in {
    val workflow = Workflow.of(WorkflowPath("CONTROLLER-WORKFLOW"),
      Options(
        stopOnFailure = Some(true),
        block = Workflow.of(Fail(Some(expr("'TEST-FAILURE'"))))))
    withTemporaryItem(workflow) { workflow =>
      val orderId = OrderId("STOP-ON-FAILURE-AT-CONTROLLER")
      controller.api.addOrder(FreshOrder(orderId, workflow.path)).await(99.s).orThrow
      eventWatch.await[OrderStopped](_.key == orderId)

      val order = controller.controllerState.await(99.s).idToOrder(orderId)
      assert(order.isState[Stopped] && order.isFailed)

      controller.api
        .executeCommand(
          ResumeOrder(orderId, asSucceeded = true, position = Some(order.position.increment)))
        .await(99.s).orThrow
      eventWatch.await[OrderTerminated](_.key == orderId)

      assert(eventWatch.eventsByKey[OrderEvent](orderId) == Seq(
        OrderAdded(workflow.id),
        OrderMoved(Position(0) / "options" % 0),
        OrderStarted,
        OrderOutcomeAdded(Outcome.Failed(Some("TEST-FAILURE"))),
        OrderStopped,
        OrderResumed(Some(Position(0) / "options" % 1), asSucceeded = true),
        OrderMoved(Position(1)),
        OrderFinished()))
    }
  }

  "Two nested Options instructions" in {
    val workflow = Workflow.of(WorkflowPath("NESTED-WORKFLOW"),
      Options(
        stopOnFailure = Some(true),
        block = Workflow.of(
          Options(
            stopOnFailure = Some(false/*Switch off again*/),
            block = Workflow.of(Fail(Some(expr("'TEST-FAILURE'"))))))))
    withTemporaryItem(workflow) { workflow =>
      val orderId = OrderId("NESTED")
      controller.api.addOrder(FreshOrder(orderId, workflow.path)).await(99.s).orThrow
      eventWatch.await[OrderTerminated](_.key == orderId)

      assert(eventWatch.eventsByKey[OrderEvent](orderId) == Seq(
        OrderAdded(workflow.id),
        OrderMoved(Position(0) / "options" % 0),
        OrderMoved(Position(0) / "options" % 0 / "options" % 0),
        OrderStarted,
        OrderOutcomeAdded(Outcome.Failed(Some("TEST-FAILURE"))),
        OrderFailed(Position(0) / "options" % 0 / "options" % 0)))
    }
  }

  "Fail at Agent" in {
    val workflow = Workflow.of(WorkflowPath("AGENT-WORKFLOW"),
      Options(
        stopOnFailure = Some(true),
        block = Workflow.of(
          FailingJob.execute(agentPath))))
    withTemporaryItem(workflow) { workflow =>
      val orderId = OrderId("STOP-ON-FAILURE-AT-AGENT")
      controller.api.addOrder(FreshOrder(orderId, workflow.path)).await(99.s).orThrow
      eventWatch.await[OrderStopped](_.key == orderId)

      val order = controller.controllerState.await(99.s).idToOrder(orderId)
      assert(order.isState[Stopped] && order.isFailed && order.isDetached)

      controller.api
        .executeCommand(
          ResumeOrder(orderId, asSucceeded = true, position = Some(order.position.increment)))
        .await(99.s).orThrow
      eventWatch.await[OrderTerminated](_.key == orderId)

      assert(eventWatch.eventsByKey[OrderEvent](orderId) == Seq(
        OrderAdded(workflow.id),
        OrderMoved(Position(0) / "options" % 0),
        OrderAttachable(agentPath),
        OrderAttached(agentPath),
        OrderStarted,
        OrderProcessingStarted(Some(subagentId)),
        OrderProcessed(Outcome.Failed()),
        OrderDetachable,
        OrderDetached,
        OrderStopped,
        OrderResumed(Some(Position(0) / "options" % 1), asSucceeded = true),
        OrderMoved(Position(1)),
        OrderFinished()))
    }
  }

  "Fail while Fresh" in {
    val workflow = Workflow.of(WorkflowPath("FRESH-WORKFLOW"),
      Options(
        stopOnFailure = Some(true),
        block = Workflow.of(
          If(expr("$param"), Workflow.empty))))
    withTemporaryItem(workflow) { workflow =>
      val orderId = OrderId("FAIL-WHILE-FRESH-AT-AGENT")
      controller.api.addOrder(FreshOrder(orderId, workflow.path)).await(99.s).orThrow
      eventWatch.await[OrderStopped](_.key == orderId)

      val order = controller.controllerState.await(99.s).idToOrder(orderId)
      assert(order.isState[StoppedWhileFresh] && order.isFailed && order.isDetached)

      val historyOperations = Seq(
        OrderResumed.AppendHistoricOutcome(
          HistoricOutcome(
            order.position,
            Outcome.Succeeded(Map(
              "param" -> BooleanValue(true))))))
      controller.api
        .executeCommand(
          ResumeOrder(orderId, historyOperations = historyOperations))
        .await(99.s).orThrow
      eventWatch.await[OrderTerminated](_.key == orderId)

      assert(eventWatch.eventsByKey[OrderEvent](orderId) == Seq(
        OrderAdded(workflow.id),
        OrderOutcomeAdded(Outcome.Disrupted(Outcome.Disrupted.Other(Problem(
          "No such named value: param")))),
        OrderStopped,
        OrderResumed(historyOperations = historyOperations),
        OrderMoved(Position(1)),
        OrderStarted,
        OrderFinished()))
    }
  }
}

object StopOnFailureTest {
  private val agentPath = AgentPath("AGENT")
  private val subagentId = toLocalSubagentId(agentPath)
}
