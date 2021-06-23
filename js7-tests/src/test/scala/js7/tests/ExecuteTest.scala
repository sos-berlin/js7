package js7.tests

import java.nio.file.Files.{createTempFile, delete}
import java.util.regex.Pattern
import js7.base.configutils.Configs._
import js7.base.io.file.FileUtils.syntax.RichPath
import js7.base.io.process.ReturnCode
import js7.base.log.Logger
import js7.base.problem.Checked._
import js7.base.problem.Problem
import js7.base.system.OperatingSystem.isWindows
import js7.base.utils.ScalaUtils.syntax.RichPartialFunction
import js7.data.agent.AgentPath
import js7.data.item.VersionId
import js7.data.job.{AbsolutePathExecutable, CommandLineExecutable, CommandLineParser, Executable, InternalExecutable, JobResource, JobResourcePath, ProcessExecutable, RelativePathExecutable, ReturnCodeMeaning, ShellScriptExecutable}
import js7.data.order.OrderEvent.{OrderFailed, OrderFinished, OrderProcessed, OrderStdWritten, OrderStdoutWritten}
import js7.data.order.{FreshOrder, OrderEvent, OrderId, Outcome}
import js7.data.value.expression.Expression.{NamedValue, NumericConstant, StringConstant}
import js7.data.value.expression.ExpressionParser
import js7.data.value.{NamedValues, NumberValue, StringValue, Value}
import js7.data.workflow.instructions.Execute
import js7.data.workflow.instructions.executable.WorkflowJob
import js7.data.workflow.{OrderRequirements, Workflow, WorkflowId, WorkflowParameter, WorkflowParameters, WorkflowParser, WorkflowPath, WorkflowPrinter}
import js7.executor.OrderProcess
import js7.executor.internal.InternalJob
import js7.tests.ExecuteTest._
import js7.tests.testenv.ControllerAgentForScalaTest
import monix.eval.Task
import org.scalactic.source
import org.scalatest.freespec.AnyFreeSpec

final class ExecuteTest extends AnyFreeSpec with ControllerAgentForScalaTest
{
  protected val agentPaths = agentPath :: Nil
  protected val items = Seq(jobResource)
  override protected val controllerConfig = config"""
    js7.journal.remove-obsolete-files = false
    js7.controller.agent-driver.command-batch-delay = 0ms
    js7.controller.agent-driver.event-buffer-delay = 10ms"""
  override protected def agentConfig = config"""
    js7.job.execution.signed-script-injection-allowed = on
    """
  private val versionIdIterator = Iterator.from(1).map(i => VersionId(s"v$i"))
  private val workflowPathIterator = Iterator.from(1).map(i => WorkflowPath(s"WORKFLOW-$i"))
  private val orderIdIterator = Iterator.from(1).map(i => OrderId(s"🔵-$i"))
  private lazy val argScriptFile = createTempFile("ExecuteTest-arg-", ".cmd")
  private lazy val myReturnCodeScriptFile = createTempFile("ExecuteTest-myExitCode-", ".cmd")

  override def beforeAll() = {
    for (a <- directoryProvider.agents) {
     a.writeExecutable(RelativePathExecutable("TEST-SCRIPT.cmd"), returnCodeScript("myExitCode"))
    }
    argScriptFile.writeExecutable(
      if (isWindows)
        """@echo off
          |echo ARGUMENTS=/%*/
          |exit %2""".stripMargin
      else
        """echo ARGUMENTS=/$*/
          |exit $2""".stripMargin)
    myReturnCodeScriptFile.writeExecutable(returnCodeScript("myExitCode"))
    super.beforeAll()
  }

  override def afterAll() = {
    super.afterAll()
    delete(argScriptFile)
    delete(myReturnCodeScriptFile)
  }

  addExecuteTest(Execute(WorkflowJob(agentPath, ShellScriptExecutable(returnCodeScript(0)))),
    expectedOutcome = Outcome.Succeeded(NamedValues.rc(0)))

  addExecuteTest(Execute(WorkflowJob(agentPath, ShellScriptExecutable(returnCodeScript(1)))),
    expectedOutcome = Outcome.Failed(NamedValues.rc(1)))

  addExecuteTest(
    Execute(
      WorkflowJob(
        agentPath,
        ShellScriptExecutable(
          returnCodeScript(2),
          returnCodeMeaning = ReturnCodeMeaning.Success(Set(ReturnCode(2)))))),
    expectedOutcome = Outcome.Succeeded(NamedValues.rc(2)))

  addExecuteTest(Execute(WorkflowJob(agentPath, ShellScriptExecutable(returnCodeScript(44)))),
    expectedOutcome = Outcome.Failed(NamedValues.rc(44)))

  addExecuteTest(Execute(
    WorkflowJob(
      agentPath,
      ShellScriptExecutable(
        returnCodeScript("myExitCode"),
        env = Map("myExitCode" -> NumericConstant(44))))),
    expectedOutcome = Outcome.Failed(NamedValues.rc(44)))

  addExecuteTest(Execute(
    WorkflowJob(
      agentPath,
      ShellScriptExecutable(
        returnCodeScript("myExitCode"),
        env = Map("myExitCode" -> NamedValue.last("orderValue"))))),
    orderArguments = Map("orderValue" -> NumberValue(44)),
    expectedOutcome = Outcome.Failed(NamedValues.rc(44)))

  addExecuteTest(Execute(
    WorkflowJob(
      agentPath,
      ShellScriptExecutable(
        returnCodeScript("myExitCode"),
        env = Map("myExitCode" -> NamedValue.last("defaultArg"))),
      defaultArguments = Map("defaultArg" -> NumericConstant(44)))),
    expectedOutcome = Outcome.Failed(NamedValues.rc(44)))

  addExecuteTest(Execute(
    WorkflowJob(
      agentPath,
      ShellScriptExecutable(
        returnCodeScript("myExitCode"),
        env = Map("myExitCode" -> NamedValue.last("NAME"))),
      defaultArguments = Map("NAME" -> NumericConstant(99)))),  // ignored
    orderArguments = Map("NAME" -> NumberValue(44)),  // has priority
    expectedOutcome = Outcome.Failed(NamedValues.rc(44)))

  addExecuteTest(Execute(
    WorkflowJob(
      agentPath,
      RelativePathExecutable(
        "TEST-SCRIPT.cmd",
        env = Map("myExitCode" -> NumericConstant(44))))),
    expectedOutcome = Outcome.Failed(NamedValues.rc(44)))

  addExecuteTest(Execute(
    WorkflowJob(
      agentPath,
      AbsolutePathExecutable(
        myReturnCodeScriptFile.toString,
        env = Map("myExitCode" -> NumericConstant(44))))),
    expectedOutcome = Outcome.Failed(NamedValues.rc(44)))

  addExecuteTest(Execute(
    WorkflowJob(
      agentPath,
      CommandLineExecutable(
        CommandLineParser.parse(s"""'$argScriptFile' ARG1-DUMMY 44""").orThrow))),
    expectedOutcome = Outcome.Failed(NamedValues.rc(44)))

  addExecuteTest(Execute(
    WorkflowJob(
      agentPath,
      CommandLineExecutable(
        CommandLineParser.parse(s"""'$myReturnCodeScriptFile'""").orThrow,
        env = Map("myExitCode" -> NamedValue.last("orderValue"))))),
    orderArguments = Map("orderValue" -> NumberValue(44)),
    expectedOutcome = Outcome.Failed(NamedValues.rc(44)))

  addExecuteTest(Execute(
    WorkflowJob(
      agentPath,
      ShellScriptExecutable(
        returnCodeScript("SCHEDULER_PARAM_MYEXITCODE"),
        v1Compatible = true))),
    orderArguments = Map("myExitCode" -> NumberValue(44)),
    expectedOutcome = Outcome.Failed(NamedValues.rc(44)))

  addExecuteTest(Execute(
    WorkflowJob(
      agentPath,
      ShellScriptExecutable(
        returnCodeScript("myExitCode"),
        env = Map("myExitCode" -> NamedValue.last("UNKNOWN"))))),
    expectedOutcome = Outcome.Disrupted(Problem("No such named value: UNKNOWN")))

  addExecuteTest(
    Execute(
      WorkflowJob(
        agentPath,
        ShellScriptExecutable(
          returnCodeScript("myExitCode"),
          env = Map("myExitCode" -> NamedValue.last("myExitCode")),
          returnCodeMeaning = ReturnCodeMeaning.Success(Set(ReturnCode(1)))))),
    orderArguments = Map("myExitCode" -> NumberValue(1)),
    expectedOutcome = Outcome.Succeeded.rc(1))

  "Argument precedence" in {
    val executable = ShellScriptExecutable(
      returnCodeScript("myExitCode"),
      env = Map("myExitCode" -> NamedValue.last("myExitCode")))
    testWithWorkflow(
      Workflow(WorkflowPath.Anonymous,
        Vector(
          Execute.Named(WorkflowJob.Name("JOB")), // ReturnCode 1 of JOB
          Execute.Named(WorkflowJob.Name("JOB"), Map("myExitCode" -> NumericConstant(22))),
          Execute.Anonymous(WorkflowJob(
            agentPath,
            executable.copy(
              returnCodeMeaning = ReturnCodeMeaning.Success(Set(ReturnCode(33)))),
            Map("myExitCode" -> NumericConstant(33))))),
        Map(WorkflowJob.Name("JOB") ->
          WorkflowJob(
            agentPath,
            executable.copy(
              returnCodeMeaning = ReturnCodeMeaning.Success(Set(ReturnCode(11), ReturnCode(22)))),
            Map("myExitCode" -> NumericConstant(11))))),
      expectedOutcomes = Seq(
        Outcome.Succeeded.rc(11),
        Outcome.Succeeded.rc(22),
        Outcome.Succeeded.rc(33)))
  }

  "Arguments when v1Compatible include workflow defaults" in {
    testWithWorkflow(
      Workflow(WorkflowPath.Anonymous,
        Vector(
          Execute.Anonymous(WorkflowJob(agentPath,
            ShellScriptExecutable("""echo "C=FROM JOB" >>"$JS7_RETURN_VALUES" """))),
          Execute.Anonymous(WorkflowJob(agentPath,
            ShellScriptExecutable(
              """echo "A=$SCHEDULER_PARAM_A" >>"$SCHEDULER_RETURN_VALUES"
                |echo "B=$SCHEDULER_PARAM_B" >>"$SCHEDULER_RETURN_VALUES"
                |echo "C=$SCHEDULER_PARAM_C" >>"$SCHEDULER_RETURN_VALUES"
                |""".stripMargin,
              v1Compatible = true)))),
      orderRequirements = OrderRequirements(Some(WorkflowParameters(
        WorkflowParameter("A", NumberValue),
        WorkflowParameter("B", StringValue("WORKFLOW PARAMETER DEFAULT VALUE")))))),
      orderArguments = Map(
        "A" -> NumberValue(4711)),
      expectedOutcomes = Seq(
        Outcome.Succeeded(NamedValues(
          "returnCode" -> NumberValue(0),
          "C" ->  StringValue("FROM JOB"))),
        Outcome.Succeeded(NamedValues(
          "returnCode" -> NumberValue(0),
          "A" -> StringValue("4711"),
          "B" -> StringValue("WORKFLOW PARAMETER DEFAULT VALUE"),
          "C" -> StringValue("FROM JOB")/*Results from jobs are unknown*/))))
  }

  addExecuteTest(
    Execute(
      WorkflowJob(
        agentPath,
        InternalExecutable(
          classOf[TestInternalJob].getName,
          arguments = Map("ARG" -> NamedValue.last("ARG"))))),
    orderArguments = Map("ARG" -> NumberValue(100)),
    expectedOutcome = Outcome.Succeeded(NamedValues("RESULT" -> NumberValue(101))))

  "Special $js7 variables" - {
    val nameToExpression = Map(
      "ORDER_ID"            -> ExpressionParser.parse("$js7OrderId").orThrow,
      "WORKFLOW_NAME"       -> ExpressionParser.parse("$js7WorkflowPath").orThrow,
      "WORKFLOW_POSITION"   -> ExpressionParser.parse("$js7WorkflowPosition").orThrow,
      "LABEL"               -> ExpressionParser.parse("$js7Label").orThrow,
      "JOB_NAME"            -> ExpressionParser.parse("$js7JobName").orThrow,
      "JOB_EXECUTION_COUNT" -> ExpressionParser.parse("$js7JobExecutionCount").orThrow,
      "CONTROLLER_ID"       -> ExpressionParser.parse("$js7ControllerId").orThrow,
      "SCHEDULED_DATE"      -> ExpressionParser.parse("scheduledOrEmpty(format='yyyy-MM-dd HH:mm:ssZ')").orThrow,
      "JOBSTART_DATE"       -> ExpressionParser.parse("now(format='yyyy-MM-dd HH:mm:ssZ')").orThrow,
      "JOB_RESOURCE_VARIABLE" -> ExpressionParser.parse("JobResource:JOB-RESOURCE:VARIABLE").orThrow)

    "Special variables in InternalExecutable arguments" in {
      testWithSpecialVariables(
        InternalExecutable(
          classOf[ReturnArgumentsInternalJob].getName,
          arguments = nameToExpression))
    }

    "Special variables in env expressions" in {
      val script =
        if (isWindows)
          """@echo off
            |echo ORDER_ID=%ORDER_ID% >>%JS7_RETURN_VALUES%
            |echo WORKFLOW_NAME=%WORKFLOW_NAME% >>%JS7_RETURN_VALUES%
            |echo WORKFLOW_POSITION=%WORKFLOW_POSITION% >>%JS7_RETURN_VALUES%
            |echo LABEL=%LABEL% >>%JS7_RETURN_VALUES%
            |echo JOB_NAME=%JOB_NAME% >>%JS7_RETURN_VALUES%
            |echo JOB_EXECUTION_COUNT=%JOB_EXECUTION_COUNT% >>%JS7_RETURN_VALUES%
            |echo CONTROLLER_ID=%CONTROLLER_ID% >>%JS7_RETURN_VALUES%
            |echo SCHEDULED_DATE=%SCHEDULED_DATE% >>%JS7_RETURN_VALUES%
            |echo JOBSTART_DATE=%JOBSTART_DATE% >>%JS7_RETURN_VALUES%
            |echo JOB_RESOURCE_VARIABLE=%JOB_RESOURCE_VARIABLE% >>%JS7_RETURN_VALUES%
            |""".stripMargin
        else
          """#!/usr/bin/env bash
            |set -euo pipefail
            |( echo "ORDER_ID=$ORDER_ID"
            |  echo "WORKFLOW_NAME=$WORKFLOW_NAME"
            |  echo "WORKFLOW_POSITION=$WORKFLOW_POSITION"
            |  echo "LABEL=$LABEL"
            |  echo "JOB_NAME=$JOB_NAME"
            |  echo "JOB_EXECUTION_COUNT=$JOB_EXECUTION_COUNT"
            |  echo "CONTROLLER_ID=$CONTROLLER_ID"
            |  echo "SCHEDULED_DATE=$SCHEDULED_DATE"
            |  echo "JOBSTART_DATE=$JOBSTART_DATE"
            |  echo "JOB_RESOURCE_VARIABLE=$JOB_RESOURCE_VARIABLE"
            |)>>"$JS7_RETURN_VALUES"
            |""".stripMargin

      testWithSpecialVariables(ShellScriptExecutable(script, env = nameToExpression))
    }

    def testWithSpecialVariables(executable: Executable): Unit = {
      val versionId = versionIdIterator.next()
      val workflowId = workflowPathIterator.next() ~ versionId
      val jobName = WorkflowJob.Name("TEST-JOB")

      val workflow = Workflow(workflowId,
        Vector(
          "TEST-LABEL" @: Execute.Named(jobName)),
        nameToJob = Map(
          jobName -> WorkflowJob(agentPath, executable, jobResourcePaths = Seq(jobResource.path))))

      directoryProvider.updateVersionedItems(controller, workflow.id.versionId, Seq(workflow))

      val order = FreshOrder(orderIdIterator.next(), workflow.path)
      val events = controller.runOrder(order).map(_.value)
      events.collect { case OrderStdWritten(_, chunk) => logger.warn(chunk) }
      assert(events.last.isInstanceOf[OrderFinished])

      val namedValues = events.collect { case OrderProcessed(outcome) => outcome }
        .head
        .asInstanceOf[Outcome.Succeeded]
        .namedValues

      assert(namedValues contains "SCHEDULED_DATE")
      assert(namedValues contains "JOBSTART_DATE")

      def numberValue(number: Int) = executable match {
        case _: ProcessExecutable => StringValue(number.toString)  // JS7_RETURN_VALUES contains strings
        case _ => NumberValue(number)
      }
      assert(namedValues - "SCHEDULED_DATE" - "JOBSTART_DATE" - "returnCode" ==
        NamedValues(
          "ORDER_ID" -> StringValue(order.id.string),
          "WORKFLOW_NAME" -> StringValue(workflow.path.string),
          "WORKFLOW_POSITION" -> StringValue(s"${workflow.path.string}~${workflow.id.versionId.string}:0"),
          "LABEL" -> StringValue("TEST-LABEL"),
          "JOB_NAME" -> StringValue("TEST-JOB"),
          "JOB_EXECUTION_COUNT" -> numberValue(1),
          "CONTROLLER_ID" -> StringValue("Controller"),
          "JOB_RESOURCE_VARIABLE" -> StringValue("JOB-RESOURCE-VARIABLE-VALUE")))
    }

    "$js7JobExecutionCount" in {
      // $js7JobExecutionCount depends on a complete, not shortened Seq[HistoricOutcome].
      val jobName = WorkflowJob.Name("JOB")
      val workflowId = nextWorkflowId()
      val workflow = Workflow(
        workflowId,
        Seq(Execute(jobName), Execute(jobName), Execute(jobName)),
        nameToJob = Map(
          jobName -> WorkflowJob(
            agentPath,
            ShellScriptExecutable(
              if (isWindows)
                """@echo off
                  |echo jobExecutionCount=%jobExecutionCount% >>%JS7_RETURN_VALUES%""".stripMargin
              else
                """echo "jobExecutionCount=$jobExecutionCount" >>"$JS7_RETURN_VALUES"""",
              env = Map(
                "jobExecutionCount" -> NamedValue("js7JobExecutionCount"))))))
      directoryProvider.updateVersionedItems(controller, workflowId.versionId, Seq(workflow))
      val order = FreshOrder(orderIdIterator.next(), workflow.path)
      val processed = controller.runOrder(order).map(_.value)
        .collect { case o: OrderProcessed => o }
      assert(processed == Seq(
        OrderProcessed(Outcome.Succeeded(Map("jobExecutionCount" -> StringValue("1"),
          "returnCode" -> NumberValue(0)))),
        OrderProcessed(Outcome.Succeeded(Map("jobExecutionCount" -> StringValue("2"),
          "returnCode" -> NumberValue(0)))),
        OrderProcessed(Outcome.Succeeded(Map("jobExecutionCount" -> StringValue("3"),
          "returnCode" -> NumberValue(0))))))
    }
  }

  "Jobs in nested workflow" in {
    // TODO Forbid this?
    testWithWorkflow(
      WorkflowParser.parse("""
        define workflow {
          job aJob;
          job bJob;
          if (true) {
            job aJob;
            job bJob;
            define job aJob {
              execute agent="AGENT", script="exit 11", successReturnCodes=[11];
            }
          };
          define job aJob {
            execute agent="AGENT", script="exit 1", successReturnCodes=[1];
          }
          define job bJob {
            execute agent="AGENT", script="exit 2", successReturnCodes=[2];
          }
        }""").orThrow,
      expectedOutcomes = Seq(
        Outcome.Succeeded(NamedValues.rc(1)),
        Outcome.Succeeded(NamedValues.rc(2)),
        Outcome.Succeeded(NamedValues.rc(11)),
        Outcome.Succeeded(NamedValues.rc(2))))
  }

  "Command line arguments" in {
    // TODO Replace --agent-task-id= by something different (for example, PID returned by Java 9)
    def removeTaskId(string: String): String =
      Pattern.compile(""" --agent-task-id=[0-9]+-[0-9]+""").matcher(string).replaceAll("")

    val events = runWithWorkflow(
      Workflow.of(
        Execute(WorkflowJob(
          agentPath,
          CommandLineExecutable(
            CommandLineParser.parse(s"""'$argScriptFile' 1 'two' "three" $$ARG""").orThrow)))),
        orderArguments = Map("ARG" -> StringValue("ARG-VALUE")))
    val stdout = events.collect { case OrderStdoutWritten(chunk) => chunk }.mkString
    assert(removeTaskId(stdout)
      .contains("ARGUMENTS=/1 two three ARG-VALUE/"))
  }

  private def addExecuteTest(
    execute: Execute,
    orderArguments: Map[String, Value] = Map.empty,
    expectedOutcome: Outcome)
    (implicit pos: source.Position)
  : Unit =
    WorkflowPrinter.instructionToString(execute) in {
      testWithWorkflow(Workflow.of(execute), orderArguments, Seq(expectedOutcome))
    }

  private def testWithWorkflow(
    anonymousWorkflow: Workflow,
    orderArguments: Map[String, Value] = Map.empty,
    expectedOutcomes: Seq[Outcome])
  : Unit = {
    val events = runWithWorkflow(anonymousWorkflow, orderArguments)
    val outcomes = events.collect { case OrderProcessed(outcome) => outcome }
    assert(outcomes == expectedOutcomes)

    if (expectedOutcomes.last.isSucceeded) assert(events.last.isInstanceOf[OrderFinished])
    else assert(events.last.isInstanceOf[OrderFailed])
  }

  private def runWithWorkflow(
    anonymousWorkflow: Workflow,
    orderArguments: Map[String, Value] = Map.empty)
  : Seq[OrderEvent] = {
    //TODO OrderRequirements are missing: testPrintAndParse(anonymousWorkflow)
    val workflow = addWorkflow(anonymousWorkflow)
    val order = FreshOrder(orderIdIterator.next(), workflow.path, arguments = orderArguments)
    controller.runOrder(order).map(_.value)
  }

  private def addWorkflow(anonymousWorkflow: Workflow): Workflow = {
    val workflowId = nextWorkflowId()
    val workflow = anonymousWorkflow.withId(workflowId)
    directoryProvider.updateVersionedItems(controller, workflowId.versionId, Seq(workflow))
    workflow
  }

  private def nextWorkflowId(): WorkflowId =
    workflowPathIterator.next() ~ versionIdIterator.next()

  private def testPrintAndParse(anonymousWorkflow: Workflow): Unit = {
    val workflowNotation = WorkflowPrinter.print(anonymousWorkflow.withoutSource)
    val reparsedWorkflow = WorkflowParser.parse(workflowNotation).map(_.withoutSource)
    logger.debug(workflowNotation)
    assert(reparsedWorkflow == Right(anonymousWorkflow.withoutSource))
  }
}

object ExecuteTest
{
  private val logger = Logger(getClass)
  private val agentPath = AgentPath("AGENT")

  private def returnCodeScript(returnCode: Int) =
    if (isWindows) s"@exit $returnCode"
    else s"exit $returnCode"

  private def returnCodeScript(envName: String) =
    if (isWindows) s"@exit %$envName%"
    else s"""exit "$$$envName""""

  private val jobResource = JobResource(JobResourcePath("JOB-RESOURCE"),
    variables = Map(
      "VARIABLE" -> StringConstant("JOB-RESOURCE-VARIABLE-VALUE")))

  private final class TestInternalJob extends InternalJob
  {
    def toOrderProcess(step: Step) =
      OrderProcess(
        Task {
          Outcome.Completed.fromChecked(
            for (number <- step.arguments.checked("ARG").flatMap(_.asNumber)) yield
              Outcome.Succeeded(NamedValues("RESULT" -> NumberValue(number + 1))))
        })
  }

  private final class ReturnArgumentsInternalJob extends InternalJob
  {
    def toOrderProcess(step: Step) =
      OrderProcess(
        Task.pure(Outcome.Succeeded(step.arguments)))
  }
}
