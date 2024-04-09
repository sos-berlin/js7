package js7.tests

import js7.base.configutils.Configs.*
import js7.base.test.OurTestSuite
import js7.base.utils.ScalaUtils.syntax.RichPartialFunction
import js7.data.agent.AgentPath
import js7.data.job.{JobResource, JobResourcePath}
import js7.data.order.OrderEvent.{OrderFailed, OrderFinished, OrderProcessed}
import js7.data.order.{FreshOrder, OrderId, OrderOutcome}
import js7.data.value.expression.Expression.StringConstant
import js7.data.value.expression.ExpressionParser.expr
import js7.data.value.{NamedValues, NumberValue, StringValue, Value}
import js7.data.workflow.{OrderParameter, OrderParameterList, OrderPreparation, Workflow, WorkflowPath}
import js7.launcher.OrderProcess
import js7.launcher.internal.InternalJob
import js7.tests.OrderParameterTest.*
import js7.tests.testenv.ControllerAgentForScalaTest

final class OrderParameterTest extends OurTestSuite, ControllerAgentForScalaTest:
  override protected def controllerConfig = config"""
    js7.journal.remove-obsolete-files = false
    js7.controller.agent-driver.command-batch-delay = 0ms
    js7.controller.agent-driver.event-buffer-delay = 10ms
    """
  override protected def agentConfig = config"""
    js7.job.execution.signed-script-injection-allowed = on
    """
  protected def agentPaths = agentPath :: Nil
  protected def items = Seq(jobResource, workflow)

  private val orderIdIterator = Iterator.from(1).map(i => OrderId(s"🔷-$i"))

  "a" in:
    runWithWorkflowPath(
      workflow.path,
      orderArguments = Map(
        "myRequired" -> NumberValue(123)),
      expectedOutcomes = Seq(
        OrderOutcome.Succeeded(NamedValues(
          "myRequired" -> NumberValue(123),
          "myOptional" -> StringValue("DEFAULT VALUE"),
          "myOptionalAny" -> StringValue("DEFAULT VALUE"),
          "myFinal" -> StringValue("FINAL VALUE"),
          "myFinal2" -> NumberValue(123)))))

  "b" in:
    runWithWorkflowPath(
      workflow.path,
      orderArguments = Map(
        "myRequired" -> NumberValue(123),
        "myOptional" -> StringValue("myOptional from order")),
      expectedOutcomes = Seq(
        OrderOutcome.Succeeded(NamedValues(
          "myRequired" -> NumberValue(123),
          "myOptional" -> StringValue("myOptional from order"),
          "myOptionalAny" -> StringValue("myOptional from order"),
          "myFinal" -> StringValue("FINAL VALUE"),
          "myFinal2" -> NumberValue(123)))))

  private def runWithWorkflowPath(
    workflowPath: WorkflowPath,
    orderArguments: Map[String, Value] = Map.empty,
    expectedOutcomes: Seq[OrderOutcome])
  : Unit =
    val order = FreshOrder(orderIdIterator.next(), workflowPath, arguments = orderArguments)
    val events = controller.runOrder(order).map(_.value)
    val outcomes = events.collect { case OrderProcessed(outcome) => outcome }
    assert(outcomes == expectedOutcomes)

    if expectedOutcomes.last.isSucceeded then assert(events.last.isInstanceOf[OrderFinished])
    else assert(events.last.isInstanceOf[OrderFailed])


object OrderParameterTest:
  private val agentPath = AgentPath("AGENT")

  private val jobResource = JobResource(JobResourcePath("JOB-RESOURCE"),
    variables = Map(
      "VARIABLE" -> StringConstant("JOB-RESOURCE-VARIABLE-VALUE")))

  private val myRequiredParameter = OrderParameter.Required("myRequired", NumberValue)
  private val workflow = Workflow(WorkflowPath("WORKFLOW") ~ "INITIAL",
    Vector(
      ReturnArgumentsInternalJob.execute(agentPath, arguments = Map(
        "myRequired" -> expr("$myRequired"),
        "myOptional" -> expr("$myOptional"),
        "myOptionalAny" -> expr("$myOptionalAny"),
        "myFinal" -> expr("$myFinal"),
        "myFinal2" -> expr("$myFinal2")))),
    orderPreparation = OrderPreparation(
      OrderParameterList(Seq(
        myRequiredParameter,
        OrderParameter.Optional("myOptional", StringValue, expr("'DEFAULT VALUE'")),
        OrderParameter.Optional("myOptionalAny", StringValue, expr("$myOptional")),
        OrderParameter.Final("myFinal", expr("'FINAL VALUE'")),
        OrderParameter.Final("myFinal2", expr("$myRequired"))))))

  private final class TestInternalJob extends InternalJob:
    def toOrderProcess(step: Step) =
      OrderProcess.fromCheckedOutcome(
        for number <- step.arguments.checked("ARG").flatMap(_.asNumber) yield
          OrderOutcome.Succeeded(NamedValues("RESULT" -> NumberValue(number + 1))))

  private final class ReturnArgumentsInternalJob extends InternalJob:
    def toOrderProcess(step: Step) =
      OrderProcess.succeeded(step.arguments)
  private object ReturnArgumentsInternalJob
  extends InternalJob.Companion[ReturnArgumentsInternalJob]
