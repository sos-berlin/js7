package js7.data.value.expression.scopes

import js7.base.problem.Problem
import js7.base.problem.Problems.UnknownKeyProblem
import js7.base.time.Stopwatch.measureTime
import js7.base.time.Timestamp
import js7.base.utils.ScalaUtils.syntax.RichEither
import js7.data.agent.AgentPath
import js7.data.controller.ControllerId
import js7.data.job.{JobKey, JobResource, JobResourcePath, ShellScriptExecutable}
import js7.data.order.{Order, OrderId}
import js7.data.value.StringValue
import js7.data.value.expression.Expression.NamedValue
import js7.data.value.expression.ExpressionParser
import js7.data.value.expression.scopes.OrderScopesTest._
import js7.data.workflow.instructions.Execute
import js7.data.workflow.instructions.executable.WorkflowJob
import js7.data.workflow.{Label, Workflow, WorkflowPath}
import org.scalatest.freespec.AnyFreeSpec
import scala.collection.View

final class OrderScopesTest extends AnyFreeSpec
{
  "OrderScopes" - {
    lazy val orderScopes: OrderScopes = new OrderScopes {
      protected val controllerId = OrderScopesTest.controllerId
      protected val workflow = OrderScopesTest.workflow
      protected val order = OrderScopesTest.order
    }

    "instructionLabel" in {
      assert(orderScopes.instructionLabel == Some(Label("LABEL")))
    }

    "orderScope, for Order[Order.State], used by if, prompt and fail instructions" in {
      import orderScopes.orderScope
      assert(orderScope.parseAndEval("$orderArgument") == Right(StringValue("ORDER-ARGUMENT")))
      assert(orderScope.parseAndEval("$js7OrderId") == Right(StringValue("ORDER")))
      assert(orderScope.parseAndEval("$js7WorkflowPosition") == Right(StringValue("WORKFLOW~VERSION:0")))
      assert(orderScope.parseAndEval("$js7WorkflowPath") == Right(StringValue("WORKFLOW")))
      assert(orderScope.parseAndEval("$js7Label") == Right(StringValue("LABEL")))
      assert(orderScope.parseAndEval("$js7ControllerId") == Right(StringValue("CONTROLLER")))
      assert(orderScope.parseAndEval("scheduledOrEmpty($dateTimeFormat, $timezone)") ==
        Right(StringValue("2021-06-17 14:00")))
    }
  }

  "ProcessingOrderScopes" - {
    lazy val orderScopes: ProcessingOrderScopes = new ProcessingOrderScopes {
      protected val controllerId = OrderScopesTest.controllerId
      protected val workflow = OrderScopesTest.workflow
      protected val order = OrderScopesTest.order.copy(state = Order.Processing)
      protected val jobKey = JobKey.Named(workflow.id, jobName)
      protected val jobResources = Seq(jobResource)
    }

    "scopeForJobResourceEnv, for JobResource.env" in {
      import orderScopes.scopeForJobResourceEnv

      assert(scopeForJobResourceEnv.parseAndEval("$orderArgument") == Left(Problem("No such named value: orderArgument")))
      assert(scopeForJobResourceEnv.parseAndEval("$VARIABLE") == Left(Problem("No such named value: VARIABLE")))
      assert(scopeForJobResourceEnv.parseAndEval("$SELF") == Left(Problem("No such named value: SELF")))

      assert(scopeForJobResourceEnv.parseAndEval("$js7Label") == Right(StringValue("LABEL")))
      assert(scopeForJobResourceEnv.parseAndEval("$js7WorkflowPath") == Right(StringValue("WORKFLOW")))
    }

    "evalLazilyJobResourceVariables, for JobResource.variables" in {
      val nameToValue = orderScopes.evalLazilyJobResourceVariables(jobResource)
      assert(nameToValue.get("unknown") == None)
      assert(nameToValue.get("A") == Some(Right(StringValue("AAA"))))
      assert(nameToValue.get("UNKNOWN") == Some(Left(Problem("No such named value: unknown"))))
      assert(nameToValue.get("VARIABLE") == Some(Left(Problem("No such named value: orderArgument"))))
      assert(nameToValue.get("SELF") == Some(Left(Problem("No such named value: SELF"))))
      assert(nameToValue.get("SCHEDULED") == Some(Right(StringValue("2021-06-17 14:00:00+0200"))))
      assert(nameToValue.get("TASKSTART") == Some(Right(StringValue(
        orderScopes.nowScope.now.format("yyyy-MM-dd HH:mm:ssZ", Some("Europe/Berlin")).orThrow))))
      assert(nameToValue.get("orderArgument") == None)
      assert(nameToValue.get("js7Label") == None)
      assert(nameToValue.get("js7WorkflowPath") == None)
    }

    "scopeForJobDefaultArguments, for (WorkflowJob + Execute).defaultArguments" in {
      val nameToValue = orderScopes.evalLazilyJobDefaultArguments(Map(
        "defaultJobName" -> NamedValue("js7JobName"),
        "defaultOrderId" -> NamedValue("js7OrderId"),
        "defaultWorkflowPosition" -> expr("$js7WorkflowPosition"),
        "defaultWorkflowPath" -> expr("$js7WorkflowPath"),
        "defaultLabel" -> expr("$js7Label"),
        "defaultControllerId" -> expr("$js7ControllerId"),
        "defaultScheduled" -> expr("scheduledOrEmpty('yyyy-MM-dd', 'UTC')")))
      assert(nameToValue.get("orderArgument") == None)
      assert(nameToValue.get("defaultJobName") == Some(Right(StringValue("JOB"))))
      assert(nameToValue.get("defaultOrderId") == Some(Right(StringValue("ORDER"))))
      assert(nameToValue("defaultWorkflowPosition") == Right(StringValue("WORKFLOW~VERSION:0")))
      assert(nameToValue("defaultWorkflowPath") == Right(StringValue("WORKFLOW")))
      assert(nameToValue("defaultLabel") == Right(StringValue("LABEL")))
      assert(nameToValue("defaultControllerId") == Right(StringValue("CONTROLLER")))
      assert(nameToValue("defaultScheduled") == Right(StringValue("2021-06-17")))
    }

    "processingOrderScope, for Order[Processing]" in {
      val scope = orderScopes.processingOrderScope
      assert(scope.parseAndEval("$orderArgument") == Right(StringValue("ORDER-ARGUMENT")))
      assert(scope.parseAndEval("$js7OrderId") == Right(StringValue("ORDER")))
      assert(scope.parseAndEval("$js7Label") == Right(StringValue("LABEL")))

      assert(scope.parseAndEval("JobResource:UNKNOWN:VARIABLE") ==
        Left(UnknownKeyProblem("JobResource", "UNKNOWN")))

      assert(scope.parseAndEval("JobResource:JOB-RESOURCE:VARIABLE") ==
        Left(Problem("No such named value: orderArgument")))

      assert(scope.parseAndEval("JobResource:JOB-RESOURCE:`ORDER-ID`") ==
        Right(StringValue("ORDER")))

      assert(scope.parseAndEval("JobResource:JOB-RESOURCE:SELF") ==
        Left(Problem("No such named value: SELF")))
    }

    "Speed" in {
      implicit val scope = orderScopes.processingOrderScope
      val n = sys.props.get("test.speed").fold(10_000)(_.toInt)
      val expressionsStrings = View("$orderArgument", "$js7Label", "$js7WorkflowPosition",
        "JobResource:JOB-RESOURCE:`ORDER-ID`", "JobResource:JOB-RESOURCE:TASKSTART",
        "now($dateTimeFormat, $timezone)")
      for (exprString <- expressionsStrings) {
        val expression = expr(exprString)
        scribe.info(measureTime(n, exprString, warmUp = n) {
          expression.eval.orThrow
        }.toString)
      }
    }
  }
}

object OrderScopesTest
{
  private val controllerId = ControllerId("CONTROLLER")
  private val agentPath = AgentPath("AGENT")
  private val jobName = WorkflowJob.Name("JOB")
  private val jobResource = JobResource(JobResourcePath("JOB-RESOURCE"),
    variables = Map(
      "A" -> expr("'AAA'"),
      "VARIABLE" -> expr("$orderArgument"),
      "ORDER-ID" -> expr("$js7OrderId"),
      "SCHEDULED" -> expr("scheduledOrEmpty(format='yyyy-MM-dd HH:mm:ssZ', 'Europe/Berlin')"),
      "TASKSTART" -> expr("now(format='yyyy-MM-dd HH:mm:ssZ', 'Europe/Berlin')"),
      "UNKNOWN" -> expr("$unknown"),
      "SELF" -> expr("$SELF")))

  private val workflow = Workflow(WorkflowPath("WORKFLOW") ~ "VERSION",
    Seq(
      "LABEL" @: Execute(jobName)),
    Map(
      jobName -> WorkflowJob(agentPath, ShellScriptExecutable(":"))))

  private val order = Order(OrderId("ORDER"), workflow.id, Order.Ready,
    Map(
      "orderArgument" -> StringValue("ORDER-ARGUMENT"),
      "dateTimeFormat" -> StringValue("yyyy-MM-dd HH:mm"),
      "timezone" -> StringValue("Europe/Berlin")),
    scheduledFor = Some(Timestamp("2021-06-17T12:00:00Z")))

  private def expr(string: String) =
    ExpressionParser.parse(string).orThrow
}
