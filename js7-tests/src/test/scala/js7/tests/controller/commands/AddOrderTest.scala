package js7.tests.controller.commands

import io.circe.syntax.EncoderOps
import js7.base.circeutils.CirceUtils.RichJson
import js7.base.configutils.Configs.*
import js7.base.io.process.Stdout
import js7.base.log.Logger
import js7.base.problem.Problem
import js7.base.test.OurTestSuite
import js7.base.thread.CatsBlocking.syntax.*
import js7.base.time.ScalaTime.*
import js7.data.agent.AgentPath
import js7.data.controller.ControllerCommand.{AddOrder, DeleteOrdersWhenTerminated}
import js7.data.order.OrderEvent.{OrderAdded, OrderAttachable, OrderAttached, OrderDeleted, OrderDetachable, OrderDetached, OrderFailed, OrderFinished, OrderMoved, OrderProcessed, OrderProcessingStarted, OrderStarted, OrderStdoutWritten}
import js7.data.order.{FreshOrder, OrderId, OrderOutcome}
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

final class AddOrderTest extends OurTestSuite, ControllerAgentForScalaTest:

  protected val agentPaths = Seq(agentPath)
  protected val items = Seq(emptyWorkflow, unknownArgWorkflow, paramWorkflow)
  override protected val agentConfig = config"""
    js7.job.execution.signed-script-injection-allowed = yes
    """

  "Order in an empty workflow finishs immediately" in:
    val orderId = OrderId("EMPTY-WORKFLOW")
    assert(controller.runOrder(FreshOrder(orderId, emptyWorkflow.path)).map(_.value) == Seq(
      OrderAdded(emptyWorkflow.path ~ "INITIAL"),
      OrderStarted,
      OrderFinished()))
    execCmd(DeleteOrdersWhenTerminated(Seq(orderId)))
    eventWatch.await[OrderDeleted](_.key == orderId)

  "An unknown argument detected at Agent lets the order fail" in:
    for i <- 1 to 2 do
      val orderId = OrderId(s"UNKNOWN-ARG-$i")
      assert(controller.runOrder(FreshOrder(orderId, unknownArgWorkflow.path)).map(_.value) == Seq(
        OrderAdded(unknownArgWorkflow.path ~ "INITIAL"),
        OrderAttachable(agentPath),
        OrderAttached(agentPath),
        OrderStarted,
        OrderProcessingStarted(subagentId),
        OrderProcessed(OrderOutcome.Disrupted(Problem("No such named value: unknownString"))),
        OrderDetachable,
        OrderDetached,
        OrderFailed(Position(0))))

  "Add order without arguments to workflow requiring some" in:
    val orderId = OrderId("NO-PARAMETERS")
    val added = controller.api.executeCommand(AddOrder(FreshOrder(orderId, paramWorkflow.path)))
      .await(99.s)
    assert(added == Left(MissingOrderArgumentProblem(numberParameter)))

  "Add order with required arguments" in:
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
        OrderProcessed(OrderOutcome.succeeded),
        OrderMoved(Position(1)),
        OrderDetachable,
        OrderDetached,
        OrderFinished()))


object AddOrderTest:
  private val logger = Logger[this.type]
  private val agentPath = AgentPath("AGENT")
  private val subagentId = toLocalSubagentId(agentPath)
  private val emptyWorkflow = Workflow.of(WorkflowPath("EMPTY"))
  private val stringParameter = OrderParameter.Optional("myString", StringValue, StringConstant("DEFAULT"))
  private val numberParameter = OrderParameter.Required("myNumber", NumberValue)

  private val unknownArgWorkflow = Workflow(WorkflowPath("UNKNOWN-ARG"),
    instructions = Vector(
      EmptyJob.execute(agentPath,
        arguments = Map("string" -> NamedValue("unknownString")))))

  private class EchoJob extends InternalJob:
    def toOrderProcess(step: Step) =
      OrderProcess(step
        .write(
          Stdout,
          s"STRING=${step.arguments("STRING").convertToString}\n" +
            s"NUMBER=${step.arguments("NUMBER").convertToString}\n")
        .as(OrderOutcome.succeeded))
  private object EchoJob extends InternalJob.Companion[EchoJob]

  private val paramWorkflow = Workflow(WorkflowPath("PARAMETERIZED-WORKFLOW"),
    instructions = Vector(
      EchoJob.execute(agentPath, arguments = Map(
        "STRING" -> NamedValue("myString"),
        "NUMBER" -> NamedValue("myNumber")))),
    orderPreparation = OrderPreparation(OrderParameterList(
      stringParameter,
      numberParameter)))
