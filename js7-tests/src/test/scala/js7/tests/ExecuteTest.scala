package js7.tests

import java.nio.file.Files.{createTempFile, delete}
import java.util.regex.Pattern
import js7.base.problem.Checked._
import js7.base.problem.Problem
import js7.base.system.OperatingSystem.isWindows
import js7.common.configutils.Configs._
import js7.common.scalautil.FileUtils.syntax.RichPath
import js7.common.scalautil.Logger
import js7.data.agent.AgentId
import js7.data.item.VersionId
import js7.data.job.{AbsolutePathExecutable, CommandLineExecutable, CommandLineParser, RelativePathExecutable, ReturnCode, ScriptExecutable}
import js7.data.order.OrderEvent.{OrderFailed, OrderFinished, OrderProcessed, OrderStdoutWritten}
import js7.data.order.{FreshOrder, OrderEvent, OrderId, Outcome}
import js7.data.value.expression.Expression.{NamedValue, NumericConstant, ObjectExpression}
import js7.data.value.{NamedValues, NumberValue, StringValue, Value}
import js7.data.workflow.instructions.executable.WorkflowJob
import js7.data.workflow.instructions.{Execute, ReturnCodeMeaning}
import js7.data.workflow.{Workflow, WorkflowParser, WorkflowPath, WorkflowPrinter}
import js7.tests.ExecuteTest._
import js7.tests.testenv.ControllerAgentForScalaTest
import org.scalatest.freespec.AnyFreeSpec

final class ExecuteTest extends AnyFreeSpec with ControllerAgentForScalaTest
{
  protected val agentIds = agentId :: Nil
  protected val versionedItems = Nil
  override protected val controllerConfig = config"""
    js7.web.server.auth.public = on
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
     a.writeExecutable(RelativePathExecutable("TEST-SCRIPT.cmd"), returnCodeScript(variableRef("myExitCode")))
    }
    argScriptFile.writeExecutable(
      if (isWindows)
        """@echo off
          |echo ARGUMENTS=/%*/
          |exit %2""".stripMargin
      else
        """echo ARGUMENTS=/$*/
          |exit $2""".stripMargin)
    myReturnCodeScriptFile.writeExecutable(returnCodeScript(variableRef("myExitCode")))
    super.beforeAll()
  }

  override def afterAll() = {
    super.afterAll()
    delete(argScriptFile)
    delete(myReturnCodeScriptFile)
  }

  addExecuteTest(Execute(WorkflowJob(agentId, ScriptExecutable(returnCodeScript("0")))),
    expectedOutcome = Outcome.Succeeded(NamedValues.rc(0)))

  addExecuteTest(Execute(WorkflowJob(agentId, ScriptExecutable(returnCodeScript("1")))),
    expectedOutcome = Outcome.Failed(NamedValues.rc(1)))

  addExecuteTest(
    Execute(
      WorkflowJob(agentId, ScriptExecutable(returnCodeScript("2")),
      returnCodeMeaning = ReturnCodeMeaning.Success(Set(ReturnCode(2))))),
    expectedOutcome = Outcome.Succeeded(NamedValues.rc(2)))

  addExecuteTest(Execute(WorkflowJob(agentId, ScriptExecutable(returnCodeScript("44")))),
    expectedOutcome = Outcome.Failed(NamedValues.rc(44)))

  addExecuteTest(Execute(
    WorkflowJob(
      agentId,
      ScriptExecutable(
        returnCodeScript(variableRef("myExitCode")),
        env = ObjectExpression(Map("myExitCode" -> NumericConstant(44)))))),
    expectedOutcome = Outcome.Failed(NamedValues.rc(44)))

  addExecuteTest(Execute(
    WorkflowJob(
      agentId,
      ScriptExecutable(
        returnCodeScript(variableRef("myExitCode")),
        env = ObjectExpression(Map("myExitCode" -> NamedValue.last("orderValue")))))),
    orderArguments = Map("orderValue" -> NumberValue(44)),
    expectedOutcome = Outcome.Failed(NamedValues.rc(44)))

  addExecuteTest(Execute(
    WorkflowJob(
      agentId,
      ScriptExecutable(
        returnCodeScript(variableRef("myExitCode")),
        env = ObjectExpression(Map("myExitCode" -> NamedValue.last("defaultArg")))),
      defaultArguments = Map("defaultArg" -> NumberValue(44)))),
    expectedOutcome = Outcome.Failed(NamedValues.rc(44)))

  addExecuteTest(Execute(
    WorkflowJob(
      agentId,
      ScriptExecutable(
        returnCodeScript(variableRef("myExitCode")),
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
        returnCodeScript(variableRef("SCHEDULER_PARAM_MYEXITCODE")),
        v1Compatible = true))),
    orderArguments = Map("myExitCode" -> NumberValue(44)),
    expectedOutcome = Outcome.Failed(NamedValues.rc(44)))

  addExecuteTest(Execute(
    WorkflowJob(
      agentId,
      ScriptExecutable(
        returnCodeScript(variableRef("myExitCode")),
      env = ObjectExpression(Map("myExitCode" -> NamedValue.last("UNKNOWN")))))),
    expectedOutcome = Outcome.Disrupted(Problem("No such named value: UNKNOWN")))

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
    def removeAgentTaskId(string: String): String =
      Pattern.compile(""" --agent-task-id=[0-9]+-[0-9]+""").matcher(string).replaceAll("")

    val events = runWithWorkflow(
      toWorkflow(
        Execute(WorkflowJob(
          agentId,
          CommandLineExecutable(
            CommandLineParser.parse(s"""'$argScriptFile' 1 'two' "three" $$ARG""").orThrow)))),
        orderArguments = Map("ARG" -> StringValue("ARG-VALUE")))
    val stdout = events.collect { case OrderStdoutWritten(chunk) => chunk }.mkString
    assert(removeAgentTaskId(stdout)
      .contains("ARGUMENTS=/1 two three ARG-VALUE/"))
  }

  private def addExecuteTest(execute: Execute, orderArguments: Map[String, Value] = Map.empty, expectedOutcome: Outcome): Unit =
    WorkflowPrinter.instructionToString(execute) in {
      testWithWorkflow(toWorkflow(execute), orderArguments, Seq(expectedOutcome))
    }

  private def testWithWorkflow(anonymousWorkflow: Workflow, orderArguments: Map[String, Value] = Map.empty, expectedOutcomes: Seq[Outcome]): Unit = {
    val events = runWithWorkflow(anonymousWorkflow, orderArguments)
    val outcomes = events.collect { case OrderProcessed(outcome) => outcome }
    assert(outcomes == expectedOutcomes)

    if (expectedOutcomes.last.isSucceeded) assert(events.last.isInstanceOf[OrderFinished])
    else assert(events.last.isInstanceOf[OrderFailed])
  }

  private def runWithWorkflow(anonymousWorkflow: Workflow, orderArguments: Map[String, Value] = Map.empty): Seq[OrderEvent] = {
    testPrintAndParse(anonymousWorkflow)

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

  private def toWorkflow(execute: Execute): Workflow =
    Workflow.of(execute)

  private def variableRef(name: String) = if (isWindows) s"%$name%" else "$" + name

  private def returnCodeScript(returnCode: String) =
    if (isWindows) s"@exit $returnCode"
    else s"exit $returnCode"
}
