package js7.tests.controller.commands

import io.circe.syntax.EncoderOps
import js7.base.auth.SimpleUser
import js7.base.circeutils.CirceUtils.RichJson
import js7.base.configutils.Configs._
import js7.base.log.Logger
import js7.base.problem.Checked.Ops
import js7.base.problem.Problem
import js7.base.thread.MonixBlocking.syntax.RichTask
import js7.base.time.ScalaTime._
import js7.core.command.CommandMeta
import js7.data.agent.AgentPath
import js7.data.controller.ControllerCommand.{AddOrder, RemoveOrdersWhenTerminated}
import js7.data.job.ScriptExecutable
import js7.data.order.OrderEvent.{OrderAdded, OrderAttachable, OrderAttached, OrderDetachable, OrderDetached, OrderFailed, OrderFinished, OrderMoved, OrderProcessed, OrderProcessingStarted, OrderRemoved, OrderStarted, OrderStdoutWritten}
import js7.data.order.{FreshOrder, OrderId, Outcome}
import js7.data.value.expression.Expression.{NamedValue, ObjectExpression}
import js7.data.value.{NamedValues, NumberValue, StringValue}
import js7.data.workflow.WorkflowParameters.MissingOrderArgumentProblem
import js7.data.workflow.instructions.Execute
import js7.data.workflow.instructions.executable.WorkflowJob
import js7.data.workflow.position.Position
import js7.data.workflow.{OrderRequirements, Workflow, WorkflowParameter, WorkflowParameters, WorkflowPath}
import js7.tests.controller.commands.AddOrderTest._
import js7.tests.testenv.ControllerAgentForScalaTest
import monix.execution.Scheduler.Implicits.global
import org.scalatest.freespec.AnyFreeSpec

final class AddOrderTest extends AnyFreeSpec with ControllerAgentForScalaTest
{
  protected val agentIds = Seq(agentId)
  protected val versionedItems = Seq(emptyWorkflow, unknownArgWorkflow, paramWorkflow)
  override protected val agentConfig = config"""
    js7.job.execution.signed-script-injection-allowed = yes
    """

  private val commandMeta = CommandMeta(SimpleUser.TestAnonymous)

  "Order in an empty workflow finishs immediately" in {
    val orderId = OrderId("EMPTY-WORKFLOW")
    assert(controller.runOrder(FreshOrder(orderId, emptyWorkflow.path)).map(_.value) == Seq(
      OrderAdded(emptyWorkflow.path ~ "INITIAL"),
      OrderStarted,
      OrderFinished))
    controller.executeCommandForTest(RemoveOrdersWhenTerminated(Seq(orderId))).orThrow
    controller.eventWatch.await[OrderRemoved](_.key == orderId)
  }

  "An unknown argument detected at Agent lets the order fail" in {
    for (i <- 1 to 2) {
      val orderId = OrderId(s"UNKNOWN-ARG-$i")
      assert(controller.runOrder(FreshOrder(orderId, unknownArgWorkflow.path)).map(_.value) == Seq(
        OrderAdded(unknownArgWorkflow.path ~ "INITIAL"),
        OrderAttachable(agentId),
        OrderAttached(agentId),
        OrderStarted,
        OrderProcessingStarted,
        OrderProcessed(Outcome.Disrupted(Problem("No such named value: myString"))),
        OrderDetachable,
        OrderDetached,
        OrderFailed(Position(0))))
    }
  }

  "Add order without arguments to workflow requiring some" in {
    val orderId = OrderId("NO-PARAMETERS")
    val added = controller.executeCommand(AddOrder(FreshOrder(orderId, paramWorkflow.path)), commandMeta)
      .await(99.s)
    assert(added == Left(MissingOrderArgumentProblem(numberParameter)))
  }

  "Add order with required arguments" in {
    logger.debug("\n" + paramWorkflow.asJson.toPrettyString)
    val orderId = OrderId("PARAMETERS")
    val namedValues = NamedValues("myNumber" -> NumberValue(7))
    assert(controller.runOrder(FreshOrder(orderId, paramWorkflow.path, namedValues)).map(_.value) ==
      Seq(
        OrderAdded(paramWorkflow.path ~ "INITIAL", namedValues),
        OrderAttachable(agentId),
        OrderAttached(agentId),
        OrderStarted,
        OrderProcessingStarted,
        OrderStdoutWritten("STRING=DEFAULT\n" + "NUMBER=7\n"),
        OrderProcessed(Outcome.succeededRC0),
        OrderMoved(Position(1)),
        OrderDetachable,
        OrderDetached,
        OrderFinished))
  }
}

object AddOrderTest
{
  private val logger = Logger[this.type]
  private val agentId = AgentPath("AGENT")
  private val emptyWorkflow = Workflow.of(WorkflowPath("EMPTY"))
  private val stringParameter = WorkflowParameter("myString", StringValue, Some(StringValue("DEFAULT")))
  private val numberParameter = WorkflowParameter("myNumber", NumberValue)

  private val unknownArgWorkflow = Workflow(WorkflowPath("UNKNOWN-ARG"),
    labeledInstructions = Vector(
      Execute.Anonymous(WorkflowJob(agentId,
        ScriptExecutable(
          """#!/usr/bin/env bash
            |set -euo pipefail
            |echo "STRING=$STRING"
            |""".stripMargin,
          env = ObjectExpression(Map(
            "STRING" -> NamedValue.last("myString"))))))
    ))

  private val paramWorkflow = Workflow(WorkflowPath("PARAMETERIZED-WORKFLOW"),
    labeledInstructions = Vector(
      Execute.Anonymous(WorkflowJob(agentId,
        ScriptExecutable(
          """#!/usr/bin/env bash
            |set -euo pipefail
            |echo "STRING=$STRING"
            |echo "NUMBER=$NUMBER"
            |""".stripMargin,
          env = ObjectExpression(Map(
            "STRING" -> NamedValue.last("myString"),
            "NUMBER" -> NamedValue.last("myNumber"))))))
    ),
    orderRequirements = OrderRequirements(Some(WorkflowParameters(
      stringParameter,
      numberParameter))))
}
