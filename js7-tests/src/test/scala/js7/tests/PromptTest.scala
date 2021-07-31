package js7.tests

import js7.base.thread.MonixBlocking.syntax.RichTask
import js7.base.time.ScalaTime._
import js7.base.utils.ScalaUtils.syntax.RichEither
import js7.data.controller.ControllerCommand.{AnswerOrderPrompt, CancelOrders, ResumeOrder, SuspendOrders}
import js7.data.order.OrderEvent.{OrderCancelled, OrderFinished, OrderPromptAnswered, OrderPrompted, OrderSuspended, OrderSuspensionMarked}
import js7.data.order.{FreshOrder, Order, OrderId}
import js7.data.value.StringValue
import js7.data.value.expression.Expression.StringConstant
import js7.data.workflow.instructions.Prompt
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.tests.PromptTest._
import js7.tests.testenv.ControllerAgentForScalaTest
import monix.execution.Scheduler.Implicits.global
import org.scalatest.freespec.AnyFreeSpec

final class PromptTest extends AnyFreeSpec with ControllerAgentForScalaTest
{
  protected val agentPaths = Nil
  protected val items = Seq(workflow)

  "Prompt" in {
    val orderId = OrderId("PROMPT")
    controllerApi.addOrder(FreshOrder(orderId, workflow.path))
      .await(99.s).orThrow
    val orderPrompted = eventWatch.await[OrderPrompted](_.key == orderId).head.value.event
    assert(orderPrompted == OrderPrompted(StringValue("MY QUESTION")))
    sleep(100.ms)  // Wait until controllerState has been updated
    assert(controller.controllerState.await(99.s).idToOrder(orderId).state ==
      Order.Prompting(StringValue("MY QUESTION")))

    controllerApi.executeCommand(
      AnswerOrderPrompt(orderId/*, Outcome.Succeeded(NamedValues("myAnswer" -> StringValue("MY ANSWER")))*/)
    ).await(99.s).orThrow
    eventWatch.await[OrderFinished](_.key == orderId)
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
  private val workflow = Workflow(WorkflowPath("WORKFLOW") ~ "INITIAL", Seq(
    Prompt(StringConstant("MY QUESTION"))))
}
