package js7.data.value.expression.scopes

import js7.base.log.Logger
import js7.base.problem.Problem
import js7.base.problem.Problems.UnknownKeyProblem
import js7.base.system.OperatingSystem.PathEnvName
import js7.base.test.OurTestSuite
import js7.base.time.ScalaTime.*
import js7.base.time.Stopwatch.measureTime
import js7.base.time.Timestamp
import js7.base.utils.Collections.implicits.RichIterable
import js7.base.utils.ScalaUtils.syntax.RichEither
import js7.data.agent.AgentPath
import js7.data.controller.ControllerId
import js7.data.job.{JobKey, JobResource, JobResourcePath, ShellScriptExecutable}
import js7.data.order.{FreshOrder, HistoricOutcome, Order, OrderId, OrderOutcome}
import js7.data.subagent.SubagentId
import js7.data.value.expression.Expression.{NamedValue, StringConstant}
import js7.data.value.expression.ExpressionParser.expr
import js7.data.value.expression.Scope
import js7.data.value.expression.scopes.OrderScopesTest.*
import js7.data.value.{MissingValue, NumberValue, ObjectValue, StringValue}
import js7.data.workflow.instructions.Execute
import js7.data.workflow.instructions.executable.WorkflowJob
import js7.data.workflow.position.{Label, Position}
import js7.data.workflow.{OrderParameter, OrderParameterList, OrderPreparation, Workflow, WorkflowPath}
import scala.collection.{MapView, View}
import scala.concurrent.duration.FiniteDuration

final class OrderScopesTest extends OurTestSuite:

  "OrderScopes" - {
    lazy val orderScopes = OrderScopes(order, workflow, controllerId)

    "instructionLabel" in:
      assert(orderScopes.instructionLabel == Some(Label("LABEL-2")))

    "if, prompt and fail instructions" in:
      val scope = orderScopes.pureOrderScope
      assert(scope.parseAndEval("$orderArgument") == Right(StringValue("ORDER-ARGUMENT")))
      assert(scope.parseAndEval("scheduledOrEmpty($dateTimeFormat, $timezone)") == Right(expectedSchedule))
      assert(scope.parseAndEval("tryCount") == Right(NumberValue(0)))
      assert(scope.parseAndEval("$js7TryCount") == Right(NumberValue(0)))

      assert(scope.parseAndEval("$js7ControllerId") == Right(StringValue("CONTROLLER")))
      assert(scope.parseAndEval("$js7OrderId") == Right(StringValue("ORDER")))
      assert(scope.parseAndEval("$js7WorkflowPosition") == Right(StringValue("WORKFLOW~VERSION:2")))
      assert(scope.parseAndEval("$js7WorkflowPath") == Right(StringValue("WORKFLOW")))
      assert(scope.parseAndEval("$js7Label") == Right(StringValue("LABEL-2")))

      assert(scope.parseAndEval(s"env('$PathEnvName')") == Right(StringValue(sys.env(PathEnvName))))

      assert(scope.parseAndEval("$js7EpochMilli") == Left(Problem("No such named value: js7EpochMilli")))
      assert(scope.parseAndEval("JobResource:JOB-RESOURCE:VARIABLE") == Left(
        Problem("JobResources are not accessible here: JobResource:JOB-RESOURCE:VARIABLE")))
  }

  "ProcessingOrderScopes" - {
    lazy val orderScopes: ProcessingOrderScopes = makeProcessingOrderscopes(None, None)

    def makeProcessingOrderscopes(
      sigkillDelay: Option[FiniteDuration], timeout: Option[FiniteDuration])
    : ProcessingOrderScopes =
      new ProcessingOrderScopes:
        protected val controllerId = OrderScopesTest.controllerId
        protected val workflow = OrderScopesTest.workflow
        protected val order = OrderScopesTest.order.copy(state = Order.Processing(SubagentId("SUBAGENT")))
        protected val jobKey = JobKey.Named(workflow.id, jobName)
        protected val workflowJob = WorkflowJob(
          agentPath,
          ShellScriptExecutable("SCRIPT"),
          defaultArguments = Map.empty,
          subagentBundleId = None,
          jobResourcePaths = Nil,
          processLimit = 1,
          sigkillDelay = sigkillDelay,
          timeout = timeout,
          failOnErrWritten = false,
          admissionTimeScheme = None,
          skipIfNoAdmissionStartForOrderDay = false)
        protected val jobResources = Seq(jobResource)
        protected val fileValueScope = Scope.empty

    "JobResource.env" in:
      val scope = orderScopes.scopeForJobResources

      assert(scope.parseAndEval("$orderArgument") == Left(Problem("No such named value: orderArgument")))
      assert(scope.parseAndEval("scheduledOrEmpty('yyyy')") == Right(StringValue("2021")))
      assert(scope.parseAndEval("scheduledOrEmpty($dateTimeFormat)") == Left(
        Problem("No such named value: dateTimeFormat")))

      assert(scope.parseAndEval("$js7ControllerId") == Right(StringValue("CONTROLLER")))
      assert(scope.parseAndEval("$js7OrderId") == Right(StringValue("ORDER")))
      assert(scope.parseAndEval("$js7WorkflowPath") == Right(StringValue("WORKFLOW")))
      assert(scope.parseAndEval("$js7WorkflowPosition") == Right(StringValue("WORKFLOW~VERSION:2")))
      assert(scope.parseAndEval("$js7Label") == Right(StringValue("LABEL-2")))

      assert(scope.parseAndEval("$VARIABLE") == Left(Problem("No such named value: VARIABLE")))
      assert(scope.parseAndEval("$SELF") == Left(Problem("No such named value: SELF")))

      assert(scope.parseAndEval("$js7Label") == Right(StringValue("LABEL-2")))
      assert(scope.parseAndEval("$js7WorkflowPath") == Right(StringValue("WORKFLOW")))
      assert(scope.parseAndEval("$js7EpochMilli") == Right(
        NumberValue(orderScopes.nowScope.now.toEpochMilli)))

      // Könnte zugelassen werden ?
      assert(scope.parseAndEval("JobResource:JOB-RESOURCE:VARIABLE") == Left(
        Problem("JobResources are not accessible here: JobResource:JOB-RESOURCE:VARIABLE")))

    "evalLazilyJobResourceVariables, for JobResource.variables" in:
      val variables = orderScopes.evalLazilyJobResourceVariables(jobResource)
      assert(variables.get("unknown") == None)
      assert(variables.get("A") == Some(Right(StringValue("AAA"))))
      assert(variables.get("UNKNOWN") == Some(Left(Problem("No such named value: unknown"))))
      assert(variables.get("VARIABLE") == Some(Left(Problem("No such named value: orderArgument"))))
      assert(variables.get("SELF") == Some(Left(Problem("No such named value: SELF"))))
      assert(variables.get("SCHEDULED") == Some(Right(StringValue("2021-06-17 14:00:00+0200"))))
      assert(variables.get("NOW") == Some(Right(StringValue(
        orderScopes.nowScope.now.format("yyyy-MM-dd HH:mm:ssZ", Some("Europe/Berlin")).orThrow))))
      assert(variables.get("orderArgument") == None)
      assert(variables.get("js7Label") == None)
      assert(variables.get("js7WorkflowPath") == None)

    "scopeForJobDefaultArguments, for (WorkflowJob + Execute).defaultArguments" in:
      val defaultArguments = orderScopes.evalLazilyExecuteDefaultArguments(MapView(
        "defaultJobName" -> NamedValue("js7JobName"),
        "defaultOrderId" -> NamedValue("js7OrderId"),
        "defaultWorkflowPosition" -> expr("$js7WorkflowPosition"),
        "defaultWorkflowPath" -> expr("$js7WorkflowPath"),
        "defaultLabel" -> expr("$js7Label"),
        "defaultControllerId" -> expr("$js7ControllerId"),
        "defaultScheduled" -> expr("scheduledOrEmpty('yyyy-MM-dd', 'UTC')"),
        "defaultJobTimeout" -> expr("$js7Job.timeoutMillis"),
        "defaultJobSigkillDelay" -> expr("$js7Job.sigkillDelayMillis")))
      assert(defaultArguments.get("orderArgument") == None)
      assert(defaultArguments("defaultJobName") == Right(StringValue("JOB")))
      assert(defaultArguments("defaultOrderId") == Right(StringValue("ORDER")))
      assert(defaultArguments("defaultWorkflowPath") == Right(StringValue("WORKFLOW")))
      assert(defaultArguments("defaultWorkflowPosition") == Right(StringValue("WORKFLOW~VERSION:2")))
      assert(defaultArguments("defaultLabel") == Right(StringValue("LABEL-2")))
      assert(defaultArguments("defaultControllerId") == Right(StringValue("CONTROLLER")))
      assert(defaultArguments("defaultScheduled") == Right(StringValue("2021-06-17")))

      assert(defaultArguments("defaultJobSigkillDelay") == Right(MissingValue))
      assert(defaultArguments("defaultJobTimeout") == Right(MissingValue))

    "$job.timeoutMillis, $job.sigkillDelayMillis" in:
      val orderScopes: ProcessingOrderScopes = makeProcessingOrderscopes(
        sigkillDelay = Some(123456.µs), timeout = Some(9.s))
      val defaultArguments = orderScopes.evalLazilyExecuteDefaultArguments(MapView(
        "defaultJobTimeout" -> expr("$js7Job.timeoutMillis"),
        "defaultJobSigkillDelay" -> expr("$js7Job.sigkillDelayMillis")))
      assert(defaultArguments("defaultJobTimeout") == Right(NumberValue(9000)))
      assert(defaultArguments("defaultJobSigkillDelay") == Right(NumberValue(123)))

    "Executable.arguments, Executable.env" - {
      lazy val scope = orderScopes.processingOrderScope

      "Named values precedence" - {
        val nameToValue = order.namedValues(workflow).toMap

        "Order.nameToValue" in:
          assert(nameToValue == Map(
            "a" -> StringValue("a from order"),
            "b" -> StringValue("b from order"),
            "c" -> StringValue("c from workflow defaults"),
            "c1" -> StringValue("c from workflow defaults"),
            "d" -> StringValue("d from position 0"),
            "e" -> StringValue("e from position 1"),
            "f" -> StringValue("f from order"),
            "orderArgument" -> StringValue("ORDER-ARGUMENT"),
            "dateTimeFormat" -> StringValue("yyyy-MM-dd HH:mm"),
            "timezone" -> StringValue("Europe/Berlin")))

        "Order.v1CompatibleNamedValues" in:
          val v1NamedValues = order.v1CompatibleNamedValues(workflow)
          assert(v1NamedValues == Map(
            "a" -> StringValue("a from order"),
            "b" -> StringValue("b from order"),
            "c" -> StringValue("c from workflow defaults"),
            "c1" -> StringValue("c from workflow defaults"),
            "d" -> StringValue("d from position 0"),
            "e" -> StringValue("e from position 1"),
            "f" -> StringValue("f from position 1"),  // <-- Different to current version
            "orderArgument" -> StringValue("ORDER-ARGUMENT"),
            "dateTimeFormat" -> StringValue("yyyy-MM-dd HH:mm"),
            "timezone" -> StringValue("Europe/Berlin")))

        def check(name: String, expectedValue: String) =
          assert(nameToValue(name) == StringValue(expectedValue))
          assert(scope.parseAndEval("$" + name) == Right(StringValue(expectedValue)))

        "Name defined only as order argument" in:
          check("a","a from order")

        "Name defined as order argument with default" in:
          check("b","b from order")

        "Name defined as only as order default" in:
          check("c" ,"c from workflow defaults")

        "Name defined as only as order default, referencing another order argument" in:
          check("c1" ,"c from workflow defaults")

        "Name defined only as order step result at position 0" in:
          check("d" ,"d from position 0")

        "Name defined as order step result at positions 0 and 1" in:
          check("e" ,"e from position 1")

        "Name defined as argument and order step result" in:
          assert(scope.parseAndEval("$f") == Right(StringValue("f from order")))
      }

      "Executable.arguments, Executable.env" in:
        // Also for BlockingInternalJob.evalExpression and BlockingInternalJob.namedValue

        assert(scope.parseAndEval("$orderArgument") == Right(StringValue("ORDER-ARGUMENT")))
        assert(scope.parseAndEval("scheduledOrEmpty($dateTimeFormat, $timezone)") == Right(expectedSchedule))
        assert(scope.parseAndEval("tryCount") == Right(NumberValue(0)))
        assert(scope.parseAndEval("$js7TryCount") == Right(NumberValue(0)))

        assert(scope.parseAndEval("$js7ControllerId") == Right(StringValue("CONTROLLER")))
        assert(scope.parseAndEval("$js7OrderId") == Right(StringValue("ORDER")))
        assert(scope.parseAndEval("$js7WorkflowPath") == Right(StringValue("WORKFLOW")))
        assert(scope.parseAndEval("$js7WorkflowPosition") == Right(StringValue("WORKFLOW~VERSION:2")))
        assert(scope.parseAndEval("$js7Label") == Right(StringValue("LABEL-2")))

        assert(scope.parseAndEval(s"env('$PathEnvName')") == Right(StringValue(sys.env(PathEnvName))))

        assert(scope.parseAndEval("JobResource:JOB-RESOURCE:`ORDER-ID`") ==
          Right(StringValue("ORDER")))
        assert(scope.parseAndEval("jobResourceVariable('JOB-RESOURCE', 'ORDER-ID')") ==
          Right(StringValue("ORDER")))
        assert(scope.parseAndEval("JobResource:JOB-RESOURCE:NOW").isRight)

        assert(scope.parseAndEval("JobResource:UNKNOWN:VARIABLE") ==
          Left(UnknownKeyProblem("JobResource", "UNKNOWN")))

        assert(scope.parseAndEval("JobResource:JOB-RESOURCE:VARIABLE") ==
          Left(Problem("No such named value: orderArgument")))

        assert(scope.parseAndEval("JobResource:JOB-RESOURCE:SELF") ==
          Left(Problem("No such named value: SELF")))

        assert(scope.parseAndEval("$js7EpochMilli") == Right(
          NumberValue(orderScopes.nowScope.now.toEpochMilli)))
    }

    "Workflow.orderVariables" in:
      import OrderScopes.workflowOrderVariablesScope
      val scope = workflowOrderVariablesScope(freshOrder,
        controllerId,
        Seq(jobResource, simpleJobResource, dotJobResource).toKeyedMap(_.path),
        nowScope = NowScope(Timestamp("2021-06-21T12:33:44Z")))

      assert(scope.parseAndEval("$orderArgument") == Right(StringValue("ORDER-ARGUMENT")))
      assert(scope.parseAndEval("scheduledOrEmpty($dateTimeFormat, $timezone)") == Right(expectedSchedule))
      assert(scope.parseAndEval("tryCount") == Left(Problem("Unknown function: tryCount")))
      assert(scope.parseAndEval("$js7TryCount") == Left(Problem("No such named value: js7TryCount")))

      assert(scope.parseAndEval("$js7ControllerId") == Right(StringValue("CONTROLLER")))
      assert(scope.parseAndEval("$js7OrderId") == Right(StringValue("ORDER")))
      assert(scope.parseAndEval("$js7WorkflowPath") == Right(StringValue("WORKFLOW")))
      assert(scope.parseAndEval("$js7WorkflowPosition") == Left(
        Problem("No such named value: js7WorkflowPosition")))
      assert(scope.parseAndEval("$js7Label") == Left(Problem("No such named value: js7Label")))

      assert(scope.parseAndEval("$js7EpochMilli").isRight)

      assert(scope.parseAndEval(s"env('$PathEnvName')") == Right(StringValue(sys.env(PathEnvName))))

      assert(scope.parseAndEval("JobResource:JOB-RESOURCE:`ORDER-ID`") == Right(StringValue("ORDER")))
      assert(scope.parseAndEval("JobResource:JOB-RESOURCE:SCHEDULED") == Right(
        StringValue("2021-06-17 14:00:00+0200")))
      assert(scope.parseAndEval("JobResource:JOB-RESOURCE:NOW") == Right(
        StringValue("2021-06-21 14:33:44+0200")))

      assert(scope.parseAndEval("JobResource:UNKNOWN:VARIABLE") ==
        Left(UnknownKeyProblem("JobResource", "UNKNOWN")))

      assert(scope.parseAndEval("JobResource:JOB-RESOURCE:VARIABLE") ==
        Left(Problem("No such named value: orderArgument")))

      assert(scope.parseAndEval("JobResource:JOB-RESOURCE:SELF") ==
        Left(Problem("No such named value: SELF")))

      assert(scope.parseAndEval("$VARIABLE") == Left(Problem("No such named value: VARIABLE")))

      assert(scope.parseAndEval("JobResource:JOB-RESOURCE:VARIABLE") ==
        Left(Problem("No such named value: orderArgument")))

      // Whole JobResource as object
      assert(scope.parseAndEval("JobResource:SIMPLE") ==
        Right(ObjectValue(Map(
          "X" -> StringValue("XXX"),
          "Y" -> StringValue("YYY")))))
      assert(scope.parseAndEval("JobResource:WITH.DOT") ==
        Right(ObjectValue(Map(
          "dot" -> StringValue("DOT")))))
      assert(scope.parseAndEval("jobResourceVariables('SIMPLE')") ==
        Right(ObjectValue(Map(
          "X" -> StringValue("XXX"),
          "Y" -> StringValue("YYY")))))

      // A variable out of a JobResource
      assert(scope.parseAndEval("JobResource:SIMPLE:X") ==
        Right(StringValue("XXX")))
      assert(scope.parseAndEval("JobResource:WITH.DOT:dot") ==
        Right(StringValue("DOT")))

      // Not recommended.
      // The whole JobResource is evaluated, and the syntax is ugly
      assert(scope.parseAndEval("(JobResource:SIMPLE).X") ==
        Right(StringValue("XXX")))
      assert(scope.parseAndEval("(JobResource:WITH.DOT).dot") ==
        Right(StringValue("DOT")))
      assert(scope.parseAndEval("jobResourceVariables('SIMPLE').X") ==
        Right(StringValue("XXX")))

    "Speed" in:
      implicit val scope = orderScopes.processingOrderScope
      val n = sys.props.get("test.speed").fold(100)(_.toInt)
      val expressionsStrings = View("$orderArgument", "$js7Label", "$js7WorkflowPosition",
        "JobResource:JOB-RESOURCE:`ORDER-ID`", "JobResource:JOB-RESOURCE:NOW",
        "now($dateTimeFormat, $timezone)")
      for exprString <- expressionsStrings do
        val expression = expr(exprString)
        logger.info(measureTime(n, exprString, warmUp = n) {
          expression.eval.orThrow
        }.toString)
  }


object OrderScopesTest:
  private val logger = Logger[this.type]

  private val controllerId = ControllerId("CONTROLLER")
  private val agentPath = AgentPath("AGENT")
  private val jobName = WorkflowJob.Name("JOB")
  private val jobResource = JobResource(JobResourcePath("JOB-RESOURCE"),
    variables = Map(
      "A" -> expr("'AAA'"),
      "VARIABLE" -> expr("$orderArgument"),
      "ORDER-ID" -> expr("$js7OrderId"),
      "SCHEDULED" -> expr("scheduledOrEmpty(format='yyyy-MM-dd HH:mm:ssZ', 'Europe/Berlin')"),
      "NOW" -> expr("now(format='yyyy-MM-dd HH:mm:ssZ', 'Europe/Berlin')"),
      "UNKNOWN" -> expr("$unknown"),
      "SELF" -> expr("$SELF")))
  private val simpleJobResource = JobResource(JobResourcePath("SIMPLE"),
    variables = Map(
      "X" -> expr("'XXX'"),
      "Y" -> expr("'YYY'")))
  private val dotJobResource = JobResource(JobResourcePath("WITH.DOT"),
    variables = Map(
      "dot" -> expr("'DOT'")))

  private val workflow = Workflow(WorkflowPath("WORKFLOW") ~ "VERSION",
    Seq(
      "LABEL-0" @: Execute(jobName),
      "LABEL-1" @: Execute(jobName),
      "LABEL-2" @: Execute(jobName)),
    Map(
      jobName -> WorkflowJob(agentPath, ShellScriptExecutable(":"))),
    orderPreparation = OrderPreparation(OrderParameterList(
      // Order parameters are not checked in this test, but defaults are used.
      OrderParameter("a", StringValue),
      OrderParameter("b", StringConstant("b from workflow defaults")),
      OrderParameter("c", StringConstant("c from workflow defaults")),
      OrderParameter.Optional("c1", StringValue, NamedValue("c")),
      OrderParameter("f", StringValue),
      OrderParameter("orderArgument", StringValue),
      OrderParameter("timezone", StringValue),
      OrderParameter("dateTimeFormat", StringValue))))

  private val freshOrder = FreshOrder(OrderId("ORDER"), workflow.path,
    Map(
      "a" -> StringValue("a from order"),
      "b" -> StringValue("b from order"),
    //"c" -> Use workflow default
    //"e" -> Use HistoricOutcome
    //"f" -> Use HistoricOutcome
      "f" -> StringValue("f from order"),
      "orderArgument" -> StringValue("ORDER-ARGUMENT"),
      "dateTimeFormat" -> StringValue("yyyy-MM-dd HH:mm"),
      "timezone" -> StringValue("Europe/Berlin")),
    scheduledFor = Some(Timestamp("2021-06-17T12:00:00Z")))

  private val expectedSchedule = StringValue("2021-06-17 14:00")

  private val order = Order(freshOrder.id, workflow.id /: Position(2), Order.Ready,
    workflow.orderParameterList
      .prepareOrderArguments(freshOrder, controllerId, PartialFunction.empty, NowScope())
      .orThrow,
    scheduledFor = freshOrder.scheduledFor,
    historicOutcomes = Vector(
      HistoricOutcome(Position(0), OrderOutcome.Succeeded(Map(
        "d" -> StringValue("d from position 0"),
        "e" -> StringValue("e from position 0")))),
      HistoricOutcome(Position(1), OrderOutcome.Succeeded(Map(
        "e" -> StringValue("e from position 1"),
        "f" -> StringValue("f from position 1"))))))
