package js7.tests

import js7.base.configutils.Configs.HoconStringInterpolator
import js7.base.problem.Problem
import js7.base.thread.MonixBlocking.syntax.RichTask
import js7.base.time.ScalaTime._
import js7.data.agent.AgentPath
import js7.data.job.{InternalExecutable, JobResource, JobResourcePath}
import js7.data.order.OrderEvent.OrderFinished
import js7.data.order.{FreshOrder, OrderId, Outcome}
import js7.data.value.expression.Expression.{Argument, FunctionCall, NamedValue, NumericConstant, StringConstant}
import js7.data.value.expression.ExpressionParser.expr
import js7.data.value.{NumberValue, StringValue}
import js7.data.workflow.instructions.Execute
import js7.data.workflow.instructions.executable.WorkflowJob
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.executor.OrderProcess
import js7.executor.internal.InternalJob
import js7.tests.OrderVariablesTest._
import js7.tests.testenv.ControllerAgentForScalaTest
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import monix.reactive.Observable
import org.scalatest.Assertions._
import org.scalatest.freespec.AnyFreeSpec

final class OrderVariablesTest extends AnyFreeSpec with ControllerAgentForScalaTest
{
  override protected def agentConfig = config"""
    js7.job.execution.signed-script-injection-allowed = on"""

  protected val agentPaths = Seq(agentPath)
  protected val items = Seq(workflow, objectWorkflow, deJobResource, svJobResource)

  "Variables are copied to the order" in {
    def runOrder(jobResource: String, variableName: String, expected: String) =
      controller.runOrder(
        FreshOrder(OrderId(s"$jobResource-$variableName"), workflow.path, arguments = Map(
          "jobResource" -> StringValue(jobResource),
          "variableName" -> StringValue(variableName),
          "expected" -> StringValue(expected))
        )).map(_.value)
    val deEvents = runOrder("de", "Acer", "Ahorn")
    val svEvents = runOrder("sv", "Acer", "lönn")
    assert(deEvents.contains(OrderFinished) && svEvents.contains(OrderFinished))
  }

  "Argument and variable name sets must be disjoint" in {
    val checked = controllerApi.addOrders(Observable(
      FreshOrder(OrderId("DUPLICATE-NAME"), workflow.path, arguments = Map(
        "jobResource" -> StringValue("de"),
        "variableName" -> StringValue("Acer"),
        "PLANT" -> StringValue("THE DUPLICATE"))
      ))).await(99.s)
    assert(checked == Left(Problem("Names are duplicate in order arguments and order variables: PLANT")))
  }

  "JobResource.variables as an object" in {
    val orderId = OrderId("RESOURCE-VARIABLES-AS-OBJECT")
    val events = controller.runOrder(FreshOrder(orderId, objectWorkflow.path), delete = true)
    assert(events.map(_.value).contains(OrderFinished))
  }
}

object OrderVariablesTest
{
  private val agentPath = AgentPath("AGENT")

  // The job resources are named as language codes:
  private val deJobResource = JobResource(
    JobResourcePath("de"),
    variables = Map(
      "Acer" -> StringConstant("Ahorn")))

  private val svJobResource = JobResource(
    JobResourcePath("sv"),
    variables = Map(
      "Acer" -> StringConstant("lönn")))

  private val workflow =
    Workflow(
      WorkflowPath("WORKFLOW") ~ "INITIAL",
      Vector(Execute(
        WorkflowJob(
          agentPath,
          InternalExecutable(
            classOf[TestInternalJob].getName,
            arguments = Map(
              "myONE" -> NamedValue("ONE"),
              "myPLANT" -> NamedValue("PLANT"),
              "myExpected" -> NamedValue("expected")))))),
      orderVariables = Map(
        "ONE" -> NumericConstant(1),
        "PLANT" -> FunctionCall("jobResourceVariable", Seq(
          Argument(NamedValue("jobResource")),
          Argument(NamedValue("variableName"))))))

  private val objectWorkflow =
    Workflow(
      WorkflowPath("OBJECT-WORKFLOW") ~ "INITIAL",
      Vector(Execute(
        WorkflowJob(
          agentPath,
          InternalExecutable(
            classOf[TestInternalJob].getName,
            arguments = Map(
              "myONE" -> expr("1"),
              "myPLANT" -> expr("$de.Acer"),
              "myExpected" -> expr("'Ahorn'")))))),
      orderVariables = Map(
        "de" -> expr("JobResource:de"),
        "sv" -> expr("JobResource:sv")))

  final class TestInternalJob extends InternalJob {
    def toOrderProcess(step: Step) =
      OrderProcess(Task {
        assert(step.arguments("myONE") == NumberValue(1))
        assert(step.arguments("myPLANT") == step.arguments("myExpected"))
        Outcome.succeeded
      })
  }
}
