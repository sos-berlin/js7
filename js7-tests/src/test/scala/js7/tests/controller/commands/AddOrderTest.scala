package js7.tests.controller.commands

import io.circe.syntax.EncoderOps
import js7.base.auth.SimpleUser
import js7.base.circeutils.CirceUtils.RichJson
import js7.base.configutils.Configs.*
import js7.base.log.Logger
import js7.base.problem.Checked.Ops
import js7.base.problem.Problem
import js7.base.test.OurTestSuite
import js7.base.thread.MonixBlocking.syntax.RichTask
import js7.base.time.ScalaTime.*
import js7.core.command.CommandMeta
import js7.data.agent.AgentPath
import js7.data.controller.ControllerCommand.{AddOrder, DeleteOrdersWhenTerminated}
import js7.data.order.OrderEvent.{OrderAdded, OrderAttachable, OrderAttached, OrderDeleted, OrderDetachable, OrderDetached, OrderFailed, OrderFinished, OrderMoved, OrderProcessed, OrderProcessingStarted, OrderStarted, OrderStdoutWritten}
import js7.data.order.{FreshOrder, OrderId, Outcome}
import js7.data.value.expression.Expression.{NamedValue, StringConstant}
import js7.data.value.{NamedValues, NumberValue, StringValue}
import js7.data.workflow.OrderParameterList.MissingOrderArgumentProblem
import js7.data.workflow.position.Position
import js7.data.workflow.{OrderParameter, OrderParameterList, OrderPreparation, Workflow, WorkflowPath}
import js7.launcher.OrderProcess
import js7.launcher.internal.InternalJob
import js7.tests.controller.commands.AddOrderTest.*
import js7.tests.jobs.EmptyJob
import js7.tests.testenv.ControllerAgentForScalaTest
import js7.tests.testenv.DirectoryProvider.toLocalSubagentId
import monix.execution.Scheduler.Implicits.traced

final class AddOrderTest extends OurTestSuite with ControllerAgentForScalaTest
{
  protected val agentPaths = Seq(agentPath)
  protected val items = Seq(emptyWorkflow, unknownArgWorkflow, paramWorkflow)
  override protected val agentConfig = config"""
    js7.job.execution.signed-script-injection-allowed = yes
    """

  private val commandMeta = CommandMeta(SimpleUser.TestAnonymous)

  "Order in an empty workflow finishs immediately" in {
    val orderId = OrderId("EMPTY-WORKFLOW")
    assert(controller.runOrder(FreshOrder(orderId, emptyWorkflow.path)).map(_.value) == Seq(
      OrderAdded(emptyWorkflow.path ~ "INITIAL"),
      OrderStarted,
      OrderFinished()))
    controller.executeCommandForTest(DeleteOrdersWhenTerminated(Seq(orderId))).orThrow
    eventWatch.await[OrderDeleted](_.key == orderId)
  }

  "An unknown argument detected at Agent lets the order fail" in {
    for (i <- 1 to 2) {
      val orderId = OrderId(s"UNKNOWN-ARG-$i")
      assert(controller.runOrder(FreshOrder(orderId, unknownArgWorkflow.path)).map(_.value) == Seq(
        OrderAdded(unknownArgWorkflow.path ~ "INITIAL"),
        OrderAttachable(agentPath),
        OrderAttached(agentPath),
        OrderStarted,
        OrderProcessingStarted(subagentId),
        OrderProcessed(Outcome.Disrupted(Problem("No such named value: unknownString"))),
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
        OrderAttachable(agentPath),
        OrderAttached(agentPath),
        OrderStarted,
        OrderProcessingStarted(subagentId),
        OrderStdoutWritten("STRING=DEFAULT\n" + "NUMBER=7\n"),
        OrderProcessed(Outcome.succeeded),
        OrderMoved(Position(1)),
        OrderDetachable,
        OrderDetached,
        OrderFinished()))
  }
}

object AddOrderTest
{
  private val logger = Logger[this.type]
  private val agentPath = AgentPath("AGENT")
  private val subagentId = toLocalSubagentId(agentPath)
  private val emptyWorkflow = Workflow.of(WorkflowPath("EMPTY"))
  private val stringParameter = OrderParameter.Optional("myString", StringValue, StringConstant("DEFAULT"))
  private val numberParameter = OrderParameter.Required("myNumber", NumberValue)

  private val unknownArgWorkflow = Workflow(WorkflowPath("UNKNOWN-ARG"),
    labeledInstructions = Vector(
      EmptyJob.execute(agentPath,
        arguments = Map("string" -> NamedValue("unknownString")))))

  private class EchoJob extends InternalJob {
    def toOrderProcess(step: Step) =
      OrderProcess(step
        .outTaskObserver.send(
          s"STRING=${step.arguments("STRING").convertToString}\n" +
          s"NUMBER=${step.arguments("NUMBER").convertToString}\n")
        .as(Outcome.succeeded))
  }
  private object EchoJob extends InternalJob.Companion[EchoJob]

  private val paramWorkflow = Workflow(WorkflowPath("PARAMETERIZED-WORKFLOW"),
    labeledInstructions = Vector(
      EchoJob.execute(agentPath, arguments = Map(
        "STRING" -> NamedValue("myString"),
        "NUMBER" -> NamedValue("myNumber")))),
    orderPreparation = OrderPreparation(OrderParameterList(
      stringParameter,
      numberParameter)))
}
