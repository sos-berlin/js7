package js7.tests

import js7.base.configutils.Configs.HoconStringInterpolator
import js7.base.test.OurTestSuite
import js7.base.thread.MonixBlocking.syntax.RichTask
import js7.base.time.ScalaTime.*
import js7.data.agent.AgentPath
import js7.data.job.{JobResource, JobResourcePath}
import js7.data.order.OrderEvent.OrderFinished
import js7.data.order.{FreshOrder, OrderId, Outcome}
import js7.data.value.expression.Expression.{Argument, FunctionCall, NamedValue, NumericConstant, StringConstant}
import js7.data.value.expression.ExpressionParser.expr
import js7.data.value.{NumberValue, StringValue}
import js7.data.workflow.OrderParameterList.FinalOrderArgumentProblem
import js7.data.workflow.{OrderParameter, OrderParameterList, OrderPreparation, Workflow, WorkflowPath}
import js7.launcher.OrderProcess
import js7.launcher.internal.InternalJob
import js7.tests.WorkflowDefinedOrderVariablesTest.*
import js7.tests.testenv.ControllerAgentForScalaTest
import monix.eval.Task
import monix.execution.Scheduler.Implicits.traced
import monix.reactive.Observable
import org.scalatest.Assertions.*

final class WorkflowDefinedOrderVariablesTest extends OurTestSuite with ControllerAgentForScalaTest
{
  override protected def agentConfig = config"""
    js7.job.execution.signed-script-injection-allowed = on"""

  protected val agentPaths = Seq(agentPath)
  protected val items = Seq(workflow, objectWorkflow, deJobResource, svJobResource)

  "Variables are copied to the order" in {
    def runOrder(jobResource: String, variableName: String, expected: String) = {
      val orderId = OrderId(s"$jobResource-$variableName")
      val events = controller.runOrder(
        FreshOrder(orderId, workflow.path, arguments = Map(
          "jobResource" -> StringValue(jobResource),
          "variableName" -> StringValue(variableName),
          "expected" -> StringValue(expected))
        )).map(_.value)
      orderId -> events
    }

    val (deOrderId, deEvents) = runOrder("de", "Acer", "Ahorn")
    val (_        , svEvents) = runOrder("sv", "Acer", "lönn")

    assert(controllerState.idToOrder(deOrderId).arguments == Map(
      "jobResource" -> StringValue("de"),
      "variableName" -> StringValue("Acer"),
      "expected" -> StringValue("Ahorn"),
      "PLANT" -> StringValue("Ahorn")))
    assert(deEvents.contains(OrderFinished) && svEvents.contains(OrderFinished))
  }

  "Argument and variable name sets must be disjoint" in {
    val checked = controllerApi.addOrders(Observable(
      FreshOrder(OrderId("DUPLICATE-NAME"), workflow.path, arguments = Map(
        "jobResource" -> StringValue("de"),
        "variableName" -> StringValue("Acer"),
        "PLANT" -> StringValue("THE DUPLICATE"))
      ))).await(99.s)
    assert(checked == Left(FinalOrderArgumentProblem("PLANT")))
  }

  "JobResource.variables as an object" in {
    val orderId = OrderId("RESOURCE-VARIABLES-AS-OBJECT")
    val events = controller.runOrder(
      FreshOrder(orderId, objectWorkflow.path, deleteWhenTerminated = true))
    assert(events.map(_.value).contains(OrderFinished))
  }
}

object WorkflowDefinedOrderVariablesTest
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
      Vector(
        TestJob.execute(agentPath, arguments = Map(
          "myONE" -> NamedValue("ONE"),
          "myPLANT" -> NamedValue("PLANT"),
          "myExpected" -> NamedValue("expected")))),
      orderPreparation = OrderPreparation(OrderParameterList(
        Seq(
          OrderParameter.Final("ONE", NumericConstant(1)),
            OrderParameter.Final("PLANT", FunctionCall("jobResourceVariable", Seq(
            Argument(NamedValue("jobResource")),
            Argument(NamedValue("variableName")))))),
        allowUndeclared = true)))

  private val objectWorkflow =
    Workflow(
      WorkflowPath("OBJECT-WORKFLOW") ~ "INITIAL",
      Vector(
        TestJob.execute(agentPath, arguments = Map(
          "myONE" -> expr("1"),
          "myPLANT" -> expr("$de.Acer"),
          "myExpected" -> expr("'Ahorn'")))),
      orderPreparation = OrderPreparation(OrderParameterList(
        OrderParameter.Final("de", expr("JobResource:de")),
        OrderParameter.Final("sv", expr("JobResource:sv")))))

  private class TestJob extends InternalJob {
    def toOrderProcess(step: Step) =
      OrderProcess(Task {
        assert(step.arguments("myONE") == NumberValue(1))
        assert(step.arguments("myPLANT") == step.arguments("myExpected"))
        Outcome.succeeded
      })
  }
  private object TestJob extends InternalJob.Companion[TestJob]
}
