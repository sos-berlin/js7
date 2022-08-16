package js7.tests

import js7.base.problem.Problem
import js7.base.test.Test
import js7.base.thread.MonixBlocking.syntax.RichTask
import js7.base.time.ScalaTime.*
import js7.base.utils.ScalaUtils.syntax.RichEither
import js7.data.agent.AgentPath
import js7.data.controller.ControllerCommand.{AnswerOrderPrompt, CancelOrders, ControlWorkflowPath, ResumeOrder, SuspendOrders}
import js7.data.order.OrderEvent.{OrderAdded, OrderCancelled, OrderFailed, OrderFinished, OrderMoved, OrderPromptAnswered, OrderPrompted, OrderStarted, OrderSuspended, OrderSuspensionMarked, OrderTerminated}
import js7.data.order.{FreshOrder, Order, OrderEvent, OrderId, Outcome}
import js7.data.value.StringValue
import js7.data.value.expression.Expression.StringConstant
import js7.data.value.expression.ExpressionParser.expr
import js7.data.workflow.instructions.{If, Prompt}
import js7.data.workflow.position.{Label, Position}
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.tests.PromptTest.*
import js7.tests.jobs.EmptyJob
import js7.tests.testenv.ControllerAgentForScalaTest
import monix.execution.Scheduler.Implicits.traced

final class PromptTest extends Test with ControllerAgentForScalaTest
{
  protected val agentPaths = Seq(agentPath)
  protected val items = Seq(workflow, failingWorkflow, skippedWorkflow)

  "Prompt" in {
    val orderId = OrderId("PROMPT")
    controllerApi.addOrder(FreshOrder(orderId, workflow.path))
      .await(99.s).orThrow
    val orderPrompted = eventWatch.await[OrderPrompted](_.key == orderId).head.value.event
    assert(orderPrompted == OrderPrompted(StringValue("MY QUESTION")))
    sleep(100.ms)  // Wait until controllerState has been updated
    assert(controllerState.idToOrder(orderId).state ==
      Order.Prompting(StringValue("MY QUESTION")))

    controllerApi.executeCommand(
      AnswerOrderPrompt(orderId/*, Outcome.Succeeded(NamedValues("myAnswer" -> StringValue("MY ANSWER")))*/)
    ).await(99.s).orThrow
    eventWatch.await[OrderFinished](_.key == orderId)
  }

  "AnswerOrderPrompt does not execute following If statement" in {
    val orderId = OrderId("FAILING")
    controllerApi.addOrder(FreshOrder(orderId, failingWorkflow.path))
      .await(99.s).orThrow
    eventWatch.await[OrderPrompted](_.key == orderId).head.value.event
    controllerApi.executeCommand(AnswerOrderPrompt(orderId)).await(99.s).orThrow
    eventWatch.await[OrderTerminated](_.key == orderId)
    assert(eventWatch.eventsByKey[OrderEvent](orderId) == Seq(
      OrderAdded(failingWorkflow.id),
      OrderStarted, OrderPrompted(StringValue("MY QUESTION")),
      OrderPromptAnswered(),
      OrderMoved(Position(1)),
      OrderFailed(Position(1), Some(Outcome.Disrupted(Problem("No such named value: UNKNOWN"))))))
  }

  "Prompt followed by a skipped statement" in {
    val workflowId = skippedWorkflow.id
    val orderId = OrderId("SKIPPED")
    val order = FreshOrder(OrderId("SKIPPED"), workflowId.path)
    controllerApi
      .executeCommand(ControlWorkflowPath(workflowId.path, skip = Map(
        Label("LABEL") -> true)))
      .await(99.s).orThrow
    controllerApi.addOrder(order)
      .await(99.s).orThrow
    eventWatch.await[OrderPrompted](_.key == orderId).head.value.event
    controllerApi.executeCommand(AnswerOrderPrompt(orderId)).await(99.s).orThrow
    eventWatch.await[OrderTerminated](_.key == orderId)
    assert(eventWatch.eventsByKey[OrderEvent](orderId) == Seq(
      OrderAdded(workflowId),
      OrderStarted, OrderPrompted(StringValue("MY QUESTION")),
      OrderPromptAnswered(),
      OrderMoved(Position(1)),
      OrderFailed(Position(1)/*???*/, Some(Outcome.Disrupted(Problem(
        "No such named value: UNKNOWN"))))))
  }

  "Order.Prompting is suspendible" in {
    val orderId = OrderId("PROMPT-SUSPENDIBLE")
    controllerApi.addOrder(FreshOrder(orderId, workflow.path))
      .await(99.s).orThrow
    eventWatch.await[OrderPrompted](_.key == orderId)

    controllerApi.executeCommand(SuspendOrders(Seq(orderId))).await(99.s).orThrow
    eventWatch.await[OrderSuspensionMarked](_.key == orderId)

    val eventId = eventWatch.lastAddedEventId
    controllerApi.executeCommand(AnswerOrderPrompt(orderId)).await(99.s).orThrow
    eventWatch.await[OrderPromptAnswered](_.key == orderId)
    eventWatch.await[OrderSuspended](_.key == orderId, after = eventId)

    controllerApi.executeCommand(ResumeOrder(orderId)).await(99.s).orThrow
    eventWatch.await[OrderFinished](_.key == orderId)
  }

  "Order.Prompting is cancelable" in {
    val orderId = OrderId("PROMPT-CANCELABLE")
    controllerApi.addOrder(FreshOrder(orderId, workflow.path))
      .await(99.s).orThrow
    eventWatch.await[OrderPrompted](_.key == orderId)

    controllerApi.executeCommand(CancelOrders(Seq(orderId))).await(99.s).orThrow
    eventWatch.await[OrderCancelled](_.key == orderId)
  }
}

object PromptTest
{
  private val agentPath = AgentPath("AGENT")

  private val workflow = Workflow(WorkflowPath("WORKFLOW") ~ "INITIAL", Seq(
    Prompt(StringConstant("MY QUESTION"))))

  private val failingWorkflow = Workflow(WorkflowPath("FAILING-WORKFLOW") ~ "INITIAL", Seq(
    Prompt(StringConstant("MY QUESTION")),
    If(expr("$UNKNOWN"), Workflow.empty)))

  private val skippedWorkflow = Workflow(WorkflowPath("SKIPPED-WORKFLOW") ~ "INITIAL", Seq(
    Prompt(StringConstant("MY QUESTION")),
    "LABEL" @: EmptyJob.execute(agentPath),
    If(expr("$UNKNOWN"), Workflow.empty)))
}
