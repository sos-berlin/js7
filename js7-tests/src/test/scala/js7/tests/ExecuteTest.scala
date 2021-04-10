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
import js7.data.agent.AgentId
import js7.data.item.VersionId
import js7.data.job.{AbsolutePathExecutable, CommandLineExecutable, CommandLineParser, InternalExecutable, RelativePathExecutable, ScriptExecutable}
import js7.data.order.OrderEvent.{OrderFailed, OrderFinished, OrderProcessed, OrderStdoutWritten}
import js7.data.order.{FreshOrder, OrderEvent, OrderId, Outcome}
import js7.data.value.expression.Expression.{NamedValue, NumericConstant, ObjectExpression}
import js7.data.value.{NamedValues, NumberValue, StringValue, Value}
import js7.data.workflow.instructions.executable.WorkflowJob
import js7.data.workflow.instructions.{Execute, ReturnCodeMeaning}
import js7.data.workflow.{OrderRequirements, Workflow, WorkflowParameter, WorkflowParameters, WorkflowParser, WorkflowPath, WorkflowPrinter}
import js7.executor.OrderProcess
import js7.executor.internal.InternalJob
import js7.tests.ExecuteTest._
import js7.tests.testenv.ControllerAgentForScalaTest
import monix.eval.Task
import org.scalactic.source
import org.scalatest.freespec.AnyFreeSpec

final class ExecuteTest extends AnyFreeSpec with ControllerAgentForScalaTest
{
  protected val agentIds = agentId :: Nil
  protected val versionedItems = Nil
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

  addExecuteTest(Execute(WorkflowJob(agentId, ScriptExecutable(returnCodeScript(0)))),
    expectedOutcome = Outcome.Succeeded(NamedValues.rc(0)))

  addExecuteTest(Execute(WorkflowJob(agentId, ScriptExecutable(returnCodeScript(1)))),
    expectedOutcome = Outcome.Failed(NamedValues.rc(1)))

  addExecuteTest(
    Execute(
      WorkflowJob(agentId, ScriptExecutable(returnCodeScript(2)),
      returnCodeMeaning = ReturnCodeMeaning.Success(Set(ReturnCode(2))))),
    expectedOutcome = Outcome.Succeeded(NamedValues.rc(2)))

  addExecuteTest(Execute(WorkflowJob(agentId, ScriptExecutable(returnCodeScript(44)))),
    expectedOutcome = Outcome.Failed(NamedValues.rc(44)))

  addExecuteTest(Execute(
    WorkflowJob(
      agentId,
      ScriptExecutable(
        returnCodeScript("myExitCode"),
        env = ObjectExpression(Map("myExitCode" -> NumericConstant(44)))))),
    expectedOutcome = Outcome.Failed(NamedValues.rc(44)))

  addExecuteTest(Execute(
    WorkflowJob(
      agentId,
      ScriptExecutable(
        returnCodeScript("myExitCode"),
        env = ObjectExpression(Map("myExitCode" -> NamedValue.last("orderValue")))))),
    orderArguments = Map("orderValue" -> NumberValue(44)),
    expectedOutcome = Outcome.Failed(NamedValues.rc(44)))

  addExecuteTest(Execute(
    WorkflowJob(
      agentId,
      ScriptExecutable(
        returnCodeScript("myExitCode"),
        env = ObjectExpression(Map("myExitCode" -> NamedValue.last("defaultArg")))),
      defaultArguments = Map("defaultArg" -> NumberValue(44)))),
    expectedOutcome = Outcome.Failed(NamedValues.rc(44)))

  addExecuteTest(Execute(
    WorkflowJob(
      agentId,
      ScriptExecutable(
        returnCodeScript("myExitCode"),
        env = ObjectExpression(Map("myExitCode" -> NamedValue.last("NAME")))),
      defaultArguments = Map("NAME" -> NumberValue(99)))),  // ignored
    orderArguments = Map("NAME" -> NumberValue(44)),  // has priority
    expectedOutcome = Outcome.Failed(NamedValues.rc(44)))

  addExecuteTest(Execute(
    WorkflowJob(
      agentId,
      RelativePathExecutable(
        "TEST-SCRIPT.cmd",
        env = ObjectExpression(Map("myExitCode" -> NumericConstant(44)))))),
    expectedOutcome = Outcome.Failed(NamedValues.rc(44)))

  addExecuteTest(Execute(
    WorkflowJob(
      agentId,
      AbsolutePathExecutable(
        myReturnCodeScriptFile.toString,
        env = ObjectExpression(Map("myExitCode" -> NumericConstant(44)))))),
    expectedOutcome = Outcome.Failed(NamedValues.rc(44)))

  addExecuteTest(Execute(
    WorkflowJob(
      agentId,
      CommandLineExecutable(
        CommandLineParser.parse(s"""'$argScriptFile' ARG1-DUMMY 44""").orThrow))),
    expectedOutcome = Outcome.Failed(NamedValues.rc(44)))

  addExecuteTest(Execute(
    WorkflowJob(
      agentId,
      CommandLineExecutable(
        CommandLineParser.parse(s"""'$myReturnCodeScriptFile'""").orThrow,
        env = ObjectExpression(Map("myExitCode" -> NamedValue.last("orderValue")))))),
    orderArguments = Map("orderValue" -> NumberValue(44)),
    expectedOutcome = Outcome.Failed(NamedValues.rc(44)))

  addExecuteTest(Execute(
    WorkflowJob(
      agentId,
      ScriptExecutable(
        returnCodeScript("SCHEDULER_PARAM_MYEXITCODE"),
        v1Compatible = true))),
    orderArguments = Map("myExitCode" -> NumberValue(44)),
    expectedOutcome = Outcome.Failed(NamedValues.rc(44)))

  addExecuteTest(Execute(
    WorkflowJob(
      agentId,
      ScriptExecutable(
        returnCodeScript("myExitCode"),
        env = ObjectExpression(Map("myExitCode" -> NamedValue.last("UNKNOWN")))))),
    expectedOutcome = Outcome.Disrupted(Problem("No such named value: UNKNOWN")))

  addExecuteTest(
    Execute(
      WorkflowJob(
        agentId,
        ScriptExecutable(
          returnCodeScript("myExitCode"),
          env = ObjectExpression(Map("myExitCode" -> NamedValue.last("myExitCode")))),
        returnCodeMeaning = ReturnCodeMeaning.Success(Set(ReturnCode(1))))),
    orderArguments = Map("myExitCode" -> NumberValue(1)),
    expectedOutcome = Outcome.Succeeded.rc(1))

  "Argument precedence" in {
    val executable = ScriptExecutable(
      returnCodeScript("myExitCode"),
      env = ObjectExpression(Map("myExitCode" -> NamedValue.last("myExitCode"))))
    testWithWorkflow(
      Workflow(WorkflowPath.Anonymous,
        Vector(
          Execute.Named(WorkflowJob.Name("JOB")), // ReturnCode 1 of JOB
          Execute.Named(WorkflowJob.Name("JOB"), Map("myExitCode" -> NumberValue(22))),
          Execute.Anonymous(WorkflowJob(agentId, executable, Map("myExitCode" -> NumberValue(33)),
            returnCodeMeaning = ReturnCodeMeaning.Success(Set(ReturnCode(33)))))),
        Map(WorkflowJob.Name("JOB") ->
          WorkflowJob(agentId, executable, Map("myExitCode" -> NumberValue(11)),
            returnCodeMeaning = ReturnCodeMeaning.Success(Set(ReturnCode(11), ReturnCode(22)))))),
      expectedOutcomes = Seq(
        Outcome.Succeeded.rc(11),
        Outcome.Succeeded.rc(22),
        Outcome.Succeeded.rc(33)))
  }

  "Arguments when v1Compatible include workflow defaults" in {
    testWithWorkflow(
      Workflow(WorkflowPath.Anonymous,
        Vector(
          Execute.Anonymous(WorkflowJob(agentId,
            ScriptExecutable("""echo "C=FROM JOB" >>"$JS7_RETURN_VALUES" """))),
          Execute.Anonymous(WorkflowJob(agentId,
            ScriptExecutable(
              """echo "A=$SCHEDULER_PARAM_A" >>"$SCHEDULER_RETURN_VALUES"
                |echo "B=$SCHEDULER_PARAM_B" >>"$SCHEDULER_RETURN_VALUES"
                |echo "C=$SCHEDULER_PARAM_C" >>"$SCHEDULER_RETURN_VALUES"
                |""".stripMargin,
              v1Compatible = true)))),
      orderRequirements = OrderRequirements(Some(WorkflowParameters(
        WorkflowParameter("A", NumberValue),
        WorkflowParameter("B", StringValue, Some(StringValue("WORKFLOW PARAMETER DEFAULT VALUE"))))))),
      orderArguments = Map(
        "A" -> NumberValue(4711)),
      expectedOutcomes = Seq(
        Outcome.Succeeded(NamedValues(
          "returnCode" -> NumberValue(0),
          "C" ->  StringValue("FROM JOB"))),
        Outcome.Succeeded(NamedValues(
          "returnCode" -> NumberValue(0),
          "A" ->  StringValue("4711"),
          "B" ->  StringValue("WORKFLOW PARAMETER DEFAULT VALUE"),
          "C" ->  StringValue("FROM JOB")/*Results from jobs are unknown*/))))
  }

  addExecuteTest(
    Execute(
      WorkflowJob(
        agentId,
        InternalExecutable(
          classOf[TestInternalJob].getName,
          arguments = ObjectExpression(Map("ARG" -> NamedValue.last("ARG")))))),
    orderArguments = Map("ARG" -> NumberValue(100)),
    expectedOutcome = Outcome.Succeeded(NamedValues("RESULT" -> NumberValue(101))))

  "Jobs in nested workflow" in {
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
          agentId,
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

    val versionId = versionIdIterator.next()
    val workflow = anonymousWorkflow.withId(workflowPathIterator.next() ~ versionId)
    val order = FreshOrder(orderIdIterator.next(), workflow.path, arguments = orderArguments)
    directoryProvider.updateVersionedItems(controller, versionId, Seq(workflow))

    controller.runOrder(order).map(_.value)
  }

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
  private val agentId = AgentId("AGENT")

  private def returnCodeScript(returnCode: Int) =
    if (isWindows) s"@exit $returnCode"
    else s"exit $returnCode"

  private def returnCodeScript(envName: String) =
    if (isWindows) s"@exit %$envName%"
    else s"""exit "$$$envName""""

  private final class TestInternalJob extends InternalJob
  {
    def processOrder(step: Step) =
      OrderProcess(
        Task {
          Outcome.Completed.fromChecked(
            for (number <- step.arguments.checked("ARG").flatMap(_.toNumber).map(_.number)) yield
              Outcome.Succeeded(NamedValues("RESULT" -> NumberValue(number + 1))))
        })
  }
}
