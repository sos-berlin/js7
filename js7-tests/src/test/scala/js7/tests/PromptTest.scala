package js7.tests

import js7.base.thread.MonixBlocking.syntax.RichTask
import js7.base.time.ScalaTime._
import js7.base.utils.ScalaUtils.syntax.RichEither
import js7.data.controller.ControllerCommand.AnswerOrderPrompt
import js7.data.order.OrderEvent.{OrderFinished, OrderPrompted}
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
    eventWatch.await[OrderFinished](_.key == orderId).head.value.event
  }
}

object PromptTest
{
  private val workflow = Workflow(WorkflowPath("WORKFLOW") ~ "INITIAL", Seq(
    Prompt(StringConstant("MY QUESTION"))))
}
