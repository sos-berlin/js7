package js7.tests

import cats.effect.IO
import fs2.Stream
import java.nio.file.Files.{createTempFile, delete}
import js7.base.configutils.Configs.*
import js7.base.io.file.FileUtils.syntax.RichPath
import js7.base.io.process.ReturnCode
import js7.base.log.Logger
import js7.base.problem.Checked.*
import js7.base.problem.Problem
import js7.base.system.OperatingSystem.isWindows
import js7.base.test.OurTestSuite
import js7.base.thread.CatsBlocking.syntax.*
import js7.base.time.ScalaTime.*
import js7.base.time.WaitForCondition.retryUntil
import js7.base.time.WallClock
import js7.base.utils.RangeSet
import js7.base.utils.ScalaUtils.syntax.RichPartialFunction
import js7.data.agent.{AgentPath, AgentRef}
import js7.data.command.CancellationMode
import js7.data.controller.ControllerCommand.CancelOrders
import js7.data.item.BasicItemEvent.ItemAttached
import js7.data.item.VersionId
import js7.data.job.{AbsolutePathExecutable, CommandLineExecutable, CommandLineParser, Executable, JobResource, JobResourcePath, ProcessExecutable, RelativePathExecutable, ReturnCodeMeaning, ShellScriptExecutable}
import js7.data.order.OrderEvent.{OrderAttached, OrderCancelled, OrderFailed, OrderFinished, OrderProcessed, OrderProcessingStarted, OrderStdWritten, OrderStdoutWritten}
import js7.data.order.OrderObstacle.{agentProcessLimitReached, jobProcessLimitReached}
import js7.data.order.{FreshOrder, Order, OrderEvent, OrderId, OrderOutcome}
import js7.data.value.expression.Expression.{NamedValue, NumericConstant, StringConstant}
import js7.data.value.expression.ExpressionParser.expr
import js7.data.value.{NamedValues, NumberValue, StringValue, Value}
import js7.data.workflow.instructions.Execute
import js7.data.workflow.instructions.executable.WorkflowJob
import js7.data.workflow.{OrderParameter, OrderParameterList, OrderPreparation, Workflow, WorkflowId, WorkflowParser, WorkflowPath, WorkflowPrinter}
import js7.launcher.OrderProcess
import js7.launcher.internal.InternalJob
import js7.tests.ExecuteTest.*
import js7.tests.jobs.SemaphoreJob
import js7.tests.testenv.{ControllerAgentForScalaTest, DirectorEnv}
import org.scalactic.source

final class ExecuteTest extends OurTestSuite, ControllerAgentForScalaTest:
  // TODO Test separate Subagent, too

  protected val agentPaths = agentPath :: Nil
  protected val items = Seq(jobResource)
  override protected val controllerConfig = config"""
    js7.auth.users.TEST-USER.permissions = [ UpdateItem ]
    js7.journal.remove-obsolete-files = false
    js7.controller.agent-driver.command-batch-delay = 0ms
    js7.controller.agent-driver.event-buffer-delay = 10ms"""
  override protected def agentConfig = config"""
    js7.job.execution.signed-script-injection-allowed = on
    """
  private val versionIdIterator = Iterator.from(1).map(i => VersionId(s"v$i"))
  private val workflowPathIterator = Iterator.from(1).map(i => WorkflowPath(s"WORKFLOW-$i"))
  private val orderIdIterator = Iterator.from(1).map(i => OrderId(s"ORDER-$i"))
  private lazy val argScriptFile = createTempFile("ExecuteTest-arg-", ".cmd")
  private lazy val myReturnCodeScriptFile = createTempFile("ExecuteTest-myExitCode-", ".cmd")

  override protected def directorEnvToToAgentRef(env: DirectorEnv) =
    super.directorEnvToToAgentRef(env).copy(processLimit = Some(3))

  override def beforeAll() =
    for a <- directoryProvider.agentEnvs do
     a.writeExecutable(RelativePathExecutable("TEST-SCRIPT.cmd"), returnCodeScript("myExitCode"))
    argScriptFile.writeUtf8Executable(
      if isWindows then
        """@echo off
          |echo ARGUMENTS=/%*/
          |exit %2""".stripMargin
      else
        """echo ARGUMENTS=/$*/
          |exit $2""".stripMargin)
    myReturnCodeScriptFile.writeUtf8Executable(returnCodeScript("myExitCode"))
    super.beforeAll()

  override def afterAll() =
    super.afterAll()
    delete(argScriptFile)
    delete(myReturnCodeScriptFile)

  addExecuteTest(Execute(WorkflowJob(agentPath, ShellScriptExecutable(returnCodeScript(0)))),
    expectedOutcome = OrderOutcome.Succeeded(NamedValues.rc(0)))

  addExecuteTest(Execute(WorkflowJob(agentPath, ShellScriptExecutable(returnCodeScript(1)))),
    expectedOutcome = OrderOutcome.Failed(NamedValues.rc(1)))

  addExecuteTest(
    Execute(
      WorkflowJob(
        agentPath,
        ShellScriptExecutable(
          returnCodeScript(2),
          returnCodeMeaning = ReturnCodeMeaning.Success(RangeSet.one(ReturnCode(2)))))),
    expectedOutcome = OrderOutcome.Succeeded(NamedValues.rc(2)))

  addExecuteTest(
    Execute(
      WorkflowJob(
        agentPath,
        ShellScriptExecutable(
          returnCodeScript(3),
          returnCodeMeaning = ReturnCodeMeaning.Success(
            RangeSet.fromRanges(Seq(
              RangeSet.Interval(ReturnCode(1), ReturnCode(5)))))))),
    expectedOutcome = OrderOutcome.Succeeded(NamedValues.rc(3)))

  addExecuteTest(Execute(WorkflowJob(agentPath, ShellScriptExecutable(returnCodeScript(44)))),
    expectedOutcome = OrderOutcome.Failed(NamedValues.rc(44)))

  addExecuteTest(Execute(
    WorkflowJob(
      agentPath,
      ShellScriptExecutable(
        returnCodeScript("myExitCode"),
        env = Map("myExitCode" -> NumericConstant(44))))),
    expectedOutcome = OrderOutcome.Failed(NamedValues.rc(44)))

  addExecuteTest(Execute(
    WorkflowJob(
      agentPath,
      ShellScriptExecutable(
        returnCodeScript("myExitCode"),
        env = Map("myExitCode" -> NamedValue("orderValue"))))),
    orderArguments = Map("orderValue" -> NumberValue(44)),
    expectedOutcome = OrderOutcome.Failed(NamedValues.rc(44)))

  addExecuteTest(Execute(
    WorkflowJob(
      agentPath,
      ShellScriptExecutable(
        returnCodeScript("myExitCode"),
        env = Map("myExitCode" -> NamedValue("defaultArg"))),
      defaultArguments = Map("defaultArg" -> NumericConstant(44)))),
    expectedOutcome = OrderOutcome.Failed(NamedValues.rc(44)))

  addExecuteTest(Execute(
    WorkflowJob(
      agentPath,
      ShellScriptExecutable(
        returnCodeScript("myExitCode"),
        env = Map("myExitCode" -> NamedValue("NAME"))),
      defaultArguments = Map("NAME" -> NumericConstant(99)))),  // ignored
    orderArguments = Map("NAME" -> NumberValue(44)),  // has priority
    expectedOutcome = OrderOutcome.Failed(NamedValues.rc(44)))

  addExecuteTest(Execute(
    WorkflowJob(
      agentPath,
      RelativePathExecutable(
        "TEST-SCRIPT.cmd",
        env = Map("myExitCode" -> NumericConstant(44))))),
    expectedOutcome = OrderOutcome.Failed(NamedValues.rc(44)))

  addExecuteTest(Execute(
    WorkflowJob(
      agentPath,
      AbsolutePathExecutable(
        myReturnCodeScriptFile.toString,
        env = Map("myExitCode" -> NumericConstant(44))))),
    expectedOutcome = OrderOutcome.Failed(NamedValues.rc(44)))

  addExecuteTest(Execute(
    WorkflowJob(
      agentPath,
      CommandLineExecutable(
        CommandLineParser.parse(s"""'$argScriptFile' ARG1-DUMMY 44""").orThrow))),
    expectedOutcome = OrderOutcome.Failed(NamedValues.rc(44)))

  addExecuteTest(Execute(
    WorkflowJob(
      agentPath,
      CommandLineExecutable(
        CommandLineParser.parse(s"""'$myReturnCodeScriptFile'""").orThrow,
        env = Map("myExitCode" -> NamedValue("orderValue"))))),
    orderArguments = Map("orderValue" -> NumberValue(44)),
    expectedOutcome = OrderOutcome.Failed(NamedValues.rc(44)))

  addExecuteTest(Execute(
    WorkflowJob(
      agentPath,
      ShellScriptExecutable(
        returnCodeScript("SCHEDULER_PARAM_MYEXITCODE"),
        v1Compatible = true))),
    orderArguments = Map("myExitCode" -> NumberValue(44)),
    expectedOutcome = OrderOutcome.Failed(NamedValues.rc(44)))

  addExecuteTest(Execute(
    WorkflowJob(
      agentPath,
      ShellScriptExecutable(
        returnCodeScript("myExitCode"),
        env = Map("myExitCode" -> NamedValue("UNKNOWN"))))),
    expectedOutcome = OrderOutcome.Disrupted(Problem("No such named value: UNKNOWN")))

  addExecuteTest(
    Execute(
      WorkflowJob(
        agentPath,
        ShellScriptExecutable(
          returnCodeScript("myExitCode"),
          env = Map("myExitCode" -> NamedValue("myExitCode")),
          returnCodeMeaning = ReturnCodeMeaning.Success(RangeSet.one(ReturnCode(1)))))),
    orderArguments = Map("myExitCode" -> NumberValue(1)),
    expectedOutcome = OrderOutcome.Succeeded.rc(1))

  "Argument precedence" in:
    val executable = ShellScriptExecutable(
      returnCodeScript("myExitCode"),
      env = Map("myExitCode" -> NamedValue("myExitCode")))
    testWithWorkflow(
      Workflow(WorkflowPath.Anonymous,
        Vector(
          Execute.Named(WorkflowJob.Name("JOB")), // ReturnCode 1 of JOB
          Execute.Named(WorkflowJob.Name("JOB"),
            Map("myExitCode" -> NumericConstant(22))),
          Execute.Anonymous(WorkflowJob(
            agentPath,
            executable.copy(
              returnCodeMeaning = ReturnCodeMeaning.Success(RangeSet.one(ReturnCode(33)))),
            Map("myExitCode" -> NumericConstant(33))))),
        Map(WorkflowJob.Name("JOB") ->
          WorkflowJob(
            agentPath,
            executable.copy(
              returnCodeMeaning = ReturnCodeMeaning.Success(RangeSet(
                ReturnCode(11), ReturnCode(22)))),
            Map("myExitCode" -> NumericConstant(11))))),
      expectedOutcomes = Seq(
        OrderOutcome.Succeeded.rc(11),
        OrderOutcome.Succeeded.rc(22),
        OrderOutcome.Succeeded.rc(33)),
      orderArguments = Map("orderExitCode" -> NumberValue(44)))

  "Execute instruction arguments refer order named values" in:
    val executable = ShellScriptExecutable(
      returnCodeScript("myExitCode"),
      env = Map("myExitCode" -> NamedValue("myExitCode")))
    testWithWorkflow(
      Workflow(WorkflowPath.Anonymous,
        Vector(
          Execute.Named(WorkflowJob.Name("JOB"),
            Map("myExitCode" -> expr("$orderExitCode"/*from Order*/))),
          Execute.Anonymous(
            WorkflowJob(
              agentPath,
              executable.copy(
                returnCodeMeaning = ReturnCodeMeaning.Success(RangeSet.one(ReturnCode(44))))),
            Map(
              "myExitCode" -> expr("$orderExitCode"/*from Order*/)))),
        Map(WorkflowJob.Name("JOB") ->
          WorkflowJob(
            agentPath,
            executable.copy(
              returnCodeMeaning = ReturnCodeMeaning.Success(RangeSet.one(ReturnCode(44)))),
            Map("myExitCode" -> NumericConstant(11))))),
      orderArguments = Map(
        "orderExitCode" -> NumberValue(44)),
      expectedOutcomes = Seq(
        OrderOutcome.Succeeded.rc(44),
        OrderOutcome.Succeeded.rc(44)))

  "Arguments when v1Compatible include workflow defaults" in:
    testWithWorkflow(
      Workflow(WorkflowPath.Anonymous,
        Vector(
          Execute.Anonymous(WorkflowJob(agentPath,
            ShellScriptExecutable(
              """echo "C=FROM FIRST JOB" >>"$JS7_RETURN_VALUES" """))),
          Execute.Anonymous(WorkflowJob(agentPath,
            ShellScriptExecutable(
              """echo "C=FROM SECOND JOB" >>"$JS7_RETURN_VALUES" """))),
          Execute.Anonymous(WorkflowJob(agentPath,
            ShellScriptExecutable(
              if isWindows then
                """@echo off
                  |echo A=%SCHEDULER_PARAM_A%>>%SCHEDULER_RETURN_VALUES%
                  |echo B=%SCHEDULER_PARAM_B%>>%SCHEDULER_RETURN_VALUES%
                  |echo C=%SCHEDULER_PARAM_C%>>%SCHEDULER_RETURN_VALUES%
                  |""".stripMargin
              else
                """echo "A=$SCHEDULER_PARAM_A" >>"$SCHEDULER_RETURN_VALUES"
                  |echo "B=$SCHEDULER_PARAM_B" >>"$SCHEDULER_RETURN_VALUES"
                  |echo "C=$SCHEDULER_PARAM_C" >>"$SCHEDULER_RETURN_VALUES"
                  |""".stripMargin,
              v1Compatible = true)))),
      orderPreparation = OrderPreparation(OrderParameterList(
        OrderParameter("A", NumberValue),
        OrderParameter("B", StringConstant("WORKFLOW PARAMETER DEFAULT VALUE"))))),
      orderArguments = Map(
        "A" -> NumberValue(4711)),
      expectedOutcomes = Seq(
        OrderOutcome.Succeeded(NamedValues(
          "returnCode" -> NumberValue(0),
          "C" ->  StringValue("FROM FIRST JOB"))),
        OrderOutcome.Succeeded(NamedValues(
          "returnCode" -> NumberValue(0),
          "C" ->  StringValue("FROM SECOND JOB"))),
        OrderOutcome.Succeeded(NamedValues(
          "returnCode" -> NumberValue(0),
          "A" -> StringValue("4711"),
          "B" -> StringValue("WORKFLOW PARAMETER DEFAULT VALUE"),
          "C" -> StringValue("FROM SECOND JOB")))))

  addExecuteTest(
    Execute(
      WorkflowJob(
        agentPath,
        TestInternalJob.executable(arguments = Map("ARG" -> NamedValue("ARG"))))),
    orderArguments = Map("ARG" -> NumberValue(100)),
    expectedOutcome = OrderOutcome.Succeeded(NamedValues("RESULT" -> NumberValue(101))))

  private val deletedEnvName =
    if isWindows then "USERNAME" else if sys.env contains "USER" then "USER" else "LANG"
  assert(sys.env(deletedEnvName).nonEmpty)  // Must exist to check deletion

  "Special $js7 variables; the ?-operators" - {
    val nameToExpression = Map(
      "ORDER_ID"            -> expr("orderId"),
      "WORKFLOW_NAME"       -> expr("workflowPath"),
      "WORKFLOW_POSITION"   -> expr("workflowPosition"),
      "LABEL"               -> expr("label"),
      "JOB_NAME"            -> expr("$js7JobName"),
      "JOB_TIMEOUT"         -> expr("""$js7Job.timeoutMillis ? "" """),
      // JOB_SIGKILL_DELAY will be unset because sigkillDelayMillis returns MissingValue
      "JOB_SIGKILL_DELAY"   -> expr("""$js7Job.sigkillDelayMillis"""),
      "JOB_EXECUTION_COUNT" -> expr("$js7JobExecutionCount"),
      "CONTROLLER_ID"       -> expr("controllerId"),
      "SCHEDULED_DATE"      -> expr("scheduledOrEmpty(format='yyyy-MM-dd HH:mm:ssZ')"),
      "JOBSTART_DATE"       -> expr("now(format='yyyy-MM-dd HH:mm:ssZ')"),
      "JOB_RESOURCE_VARIABLE" -> expr("JobResource:JOB-RESOURCE:VARIABLE"))

    "Special variables in InternalExecutable arguments" in:
      testWithSpecialVariables(
        ReturnArgumentsInternalJob.executable(arguments = nameToExpression))

    "Special variables in env expressions" in:
      val script =
        if isWindows then
          s"""@echo off
            |echo ORDER_ID=%ORDER_ID% >>%JS7_RETURN_VALUES%
            |echo WORKFLOW_NAME=%WORKFLOW_NAME% >>%JS7_RETURN_VALUES%
            |echo WORKFLOW_POSITION=%WORKFLOW_POSITION% >>%JS7_RETURN_VALUES%
            |echo LABEL=%LABEL% >>%JS7_RETURN_VALUES%
            |echo JOB_NAME=%JOB_NAME% >>%JS7_RETURN_VALUES%
            |echo JOB_TIMEOUT=%JOB_TIMEOUT% >>%JS7_RETURN_VALUES%
            |echo JOB_EXECUTION_COUNT=%JOB_EXECUTION_COUNT% >>%JS7_RETURN_VALUES%
            |echo CONTROLLER_ID=%CONTROLLER_ID% >>%JS7_RETURN_VALUES%
            |echo SCHEDULED_DATE=%SCHEDULED_DATE% >>%JS7_RETURN_VALUES%
            |echo JOBSTART_DATE=%JOBSTART_DATE% >>%JS7_RETURN_VALUES%
            |echo JOB_RESOURCE_VARIABLE=%JOB_RESOURCE_VARIABLE% >>%JS7_RETURN_VALUES%
            |if defined $deletedEnvName (
            |  echo $deletedEnvName=%$deletedEnvName% >>%JS7_RETURN_VALUES%
            |) else (
            |  echo $deletedEnvName=UNSET >>%JS7_RETURN_VALUES%
            |)
            |""".stripMargin
        else
          s"""#!/usr/bin/env bash
            |set -euo pipefail
            |if [ "$${JOB_SIGKILL_DELAY-UNSET}" != "UNSET" ]; then
            |  echo JOB_SIGKILL_DELAY should be unset
            |  exit 1
            |fi
            |( echo "ORDER_ID=$$ORDER_ID"
            |  echo "WORKFLOW_NAME=$$WORKFLOW_NAME"
            |  echo "WORKFLOW_POSITION=$$WORKFLOW_POSITION"
            |  echo "LABEL=$$LABEL"
            |  echo "JOB_NAME=$$JOB_NAME"
            |  echo "JOB_TIMEOUT=$$JOB_TIMEOUT"
            |  echo "JOB_EXECUTION_COUNT=$$JOB_EXECUTION_COUNT"
            |  echo "CONTROLLER_ID=$$CONTROLLER_ID"
            |  echo "SCHEDULED_DATE=$$SCHEDULED_DATE"
            |  echo "JOBSTART_DATE=$$JOBSTART_DATE"
            |  echo "JOB_RESOURCE_VARIABLE=$$JOB_RESOURCE_VARIABLE"
            |  echo "$deletedEnvName=$${$deletedEnvName-UNSET}"
            |)>>"$$JS7_RETURN_VALUES"
            |""".stripMargin

      val result = testWithSpecialVariables(ShellScriptExecutable(
        script,
        env = nameToExpression.updated(deletedEnvName, expr("missing"))))
      assert(result.get(deletedEnvName) == Some(StringValue("UNSET")))

    def testWithSpecialVariables(executable: Executable): NamedValues =
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
        .asInstanceOf[OrderOutcome.Succeeded]
        .namedValues

      assert(namedValues contains "SCHEDULED_DATE")
      assert(namedValues contains "JOBSTART_DATE")

      def numberValue(number: Int) = executable match
        case _: ProcessExecutable => StringValue(number.toString)  // JS7_RETURN_VALUES contains strings
        case _ => NumberValue(number)
      assert(namedValues - "SCHEDULED_DATE" - "JOBSTART_DATE" - "returnCode"  - deletedEnvName ==
        NamedValues(
          "ORDER_ID" -> StringValue(order.id.string),
          "WORKFLOW_NAME" -> StringValue(workflow.path.string),
          "WORKFLOW_POSITION" -> StringValue(s"${workflow.path.string}~${workflow.id.versionId.string}:0"),
          "LABEL" -> StringValue("TEST-LABEL"),
          "JOB_NAME" -> StringValue("TEST-JOB"),
          "JOB_TIMEOUT" -> StringValue(""),
          "JOB_EXECUTION_COUNT" -> numberValue(1),
          "CONTROLLER_ID" -> StringValue("Controller"),
          "JOB_RESOURCE_VARIABLE" -> StringValue("JOB-RESOURCE-VARIABLE-VALUE")))
      namedValues

    "$js7JobExecutionCount" in:
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
              if isWindows then
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
        OrderProcessed(OrderOutcome.Succeeded(Map("jobExecutionCount" -> StringValue("1"),
          "returnCode" -> NumberValue(0)))),
        OrderProcessed(OrderOutcome.Succeeded(Map("jobExecutionCount" -> StringValue("2"),
          "returnCode" -> NumberValue(0)))),
        OrderProcessed(OrderOutcome.Succeeded(Map("jobExecutionCount" -> StringValue("3"),
          "returnCode" -> NumberValue(0))))))
  }

  "Jobs in nested workflow" in:
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
        OrderOutcome.Succeeded(NamedValues.rc(1)),
        OrderOutcome.Succeeded(NamedValues.rc(2)),
        OrderOutcome.Succeeded(NamedValues.rc(11)),
        OrderOutcome.Succeeded(NamedValues.rc(2))))

  "Command line arguments" in:
    val events = runWithWorkflow(
      Workflow.of(
        Execute(WorkflowJob(
          agentPath,
          CommandLineExecutable(
            CommandLineParser.parse(s"""'$argScriptFile' 1 'two' "three" $$ARG""").orThrow)))),
        orderArguments = Map("ARG" -> StringValue("ARG-VALUE")))
    val stdout = events.collect { case OrderStdoutWritten(chunk) => chunk }.mkString
    assert(stdout.contains("ARGUMENTS=/1 two three ARG-VALUE/"))

  "processLimit" - {
    "WorkflowJob processLimit" in:
      val processLimit = 2
      val workflow = addWorkflow(Workflow.of(
        ParallelInternalJob.execute(agentPath, processLimit = processLimit)))
      val orderIds = for i <- 1 to processLimit yield OrderId(s"JOB-LIMIT-$i")
      val eventId = eventWatch.lastAddedEventId
      controller.api.addOrders(Stream
        .iterable(orderIds)
        .map(FreshOrder(_, workflow.path)))
        .await(99.s).orThrow
      for orderId <- orderIds do
        eventWatch.await[OrderProcessingStarted](_.key == orderId, after = eventId)
      val extraOrderId = OrderId("JOB-LIMIT-EXTRA")
      controller.api.addOrder(FreshOrder(extraOrderId, workflow.path)).await(99.s).orThrow
      eventWatch.await[OrderAttached](_.key == extraOrderId, after = eventId)
      assert(orderToObstacles(extraOrderId)(using WallClock) ==
        Right(Set(jobProcessLimitReached)))

      execCmd(CancelOrders(extraOrderId :: Nil))
      eventWatch.await[OrderCancelled](_.key == extraOrderId, after = eventId)

      execCmd:
        CancelOrders(orderIds, CancellationMode.kill(immediately = true))
      for orderId <- orderIds do
        eventWatch.await[OrderCancelled](_.key == orderId, after = eventId)

    "Agent processLimit" in:
      val jobProcessLimit = 2
      assert(agentProcessLimit == Some(3)/*last test*/)
      val firstEventId = eventWatch.lastAddedEventId
      val workflows = for _ <- 0 until 2 yield updateItem(Workflow.of(
        ParallelInternalJob.execute(agentPath, processLimit = jobProcessLimit)))
      try
        val orderIds = for i <- 0 until agentProcessLimit.get yield OrderId(s"AGENT-LIMIT-$i")
        val eventId = eventWatch.lastAddedEventId
        controller.api.addOrders:
          Stream.iterable(0 until agentProcessLimit.get).map:
            i => FreshOrder(orderIds(i), workflows(i / jobProcessLimit).path)
        .await(99.s).orThrow
        for orderId <- orderIds do
          eventWatch.await[OrderProcessingStarted](_.key == orderId, after = eventId)

        val extraOrderId = OrderId("AGENT-LIMIT-EXTRA")
        addAndExpectedLimitIsReached(extraOrderId, workflows(1).path)

        eventWatch.expect[OrderProcessed]():
          eventWatch.expect[OrderProcessingStarted]():
            ParallelInternalJob.continue(1)

        ParallelInternalJob.reset()
        for orderId <- orderIds :+ extraOrderId do
          eventWatch.await[OrderFinished](_.key == orderId, after = firstEventId)
      finally
        deleteItems(workflows.map(_.path)*)

    s"Increase Agent processLimit=4" in:
      test(increasedProcessLimit = Some(4), "B")

    s"Increase Agent processLimit=None" in:
      test(increasedProcessLimit = None, "C")

    def test(increasedProcessLimit: Option[Int], name: String) =
      eventWatch.expect[ItemAttached](_.event.key == agentPath):
        controller.api
          .updateUnsignedSimpleItems(Seq(
            controllerState.keyToItem(AgentRef)(agentPath)
              .copy(processLimit = Some(3), itemRevision = None)))
          .await(99.s).orThrow
      assert(agentProcessLimit == Some(3))
      ParallelInternalJob.reset()
      val firstEventId = eventWatch.lastAddedEventId

      val workflow = Workflow.of(
        ParallelInternalJob.execute(agentPath, processLimit = Int.MaxValue))
      withItem(workflow) { workflow =>
        val orderIds = for i <- 0 until agentProcessLimit.get yield
          OrderId(s"AGENT-LIMIT-$name-$i")
        val eventId = eventWatch.lastAddedEventId
        controller.api.addOrders(Stream
          .iterable(0 until agentProcessLimit.get)
          .map(i => FreshOrder(orderIds(i), workflow.path)))
          .await(99.s).orThrow
        for orderId <- orderIds do
          eventWatch.await[OrderProcessingStarted](_.key == orderId, after = eventId)

        val extraOrderId = OrderId(s"AGENT-LIMIT-$name-EXTRA")
        addAndExpectedLimitIsReached(extraOrderId, workflow.path)

        // Increase Agent processLimit
        eventWatch.expect[OrderProcessingStarted]():
          eventWatch.expect[ItemAttached](_.event.key == agentPath):
            controller.api
              .updateUnsignedSimpleItems(Seq(
                controllerState.keyToItem(AgentRef)(agentPath)
                  .copy(processLimit = increasedProcessLimit, itemRevision = None)))
              .await(99.s).orThrow

        ParallelInternalJob.reset()
        for orderId <- orderIds :+ extraOrderId do
          eventWatch.await[OrderFinished](_.key == orderId, after = firstEventId)
      }

    def addAndExpectedLimitIsReached(orderId: OrderId, workflowPath: WorkflowPath): Unit =
      eventWatch.expect[OrderAttached](_.key == orderId):
        controller.api.addOrder(FreshOrder(orderId, workflowPath)).await(99.s).orThrow
      retryUntil(9.s, 10.ms):
        assert(orderToObstacles(orderId)(using WallClock) == Right(Set(agentProcessLimitReached)))
      assert(controllerState.idToOrder(orderId).isState[Order.Fresh])
  }

  private def addExecuteTest(
    execute: Execute,
    orderArguments: Map[String, Value] = Map.empty,
    expectedOutcome: OrderOutcome)
    (using source.Position)
  : Unit =
    WorkflowPrinter.instructionToString(execute) in:
      testWithWorkflow(Workflow.of(execute), orderArguments, Seq(expectedOutcome))

  private def testWithWorkflow(
    anonymousWorkflow: Workflow,
    orderArguments: Map[String, Value] = Map.empty,
    expectedOutcomes: Seq[OrderOutcome])
    (using source.Position)
  : Unit =
    val events = runWithWorkflow(anonymousWorkflow, orderArguments)
    val outcomes = events.collect { case OrderProcessed(outcome) => outcome }
    assert(outcomes == expectedOutcomes)

    if expectedOutcomes.last.isSucceeded then assert(events.last.isInstanceOf[OrderFinished])
    else assert(events.last.isInstanceOf[OrderFailed])

  private def runWithWorkflow(
    anonymousWorkflow: Workflow,
    orderArguments: Map[String, Value] = Map.empty)
  : Seq[OrderEvent] =
    //TODO OrderPreparation are missing: testPrintAndParse(anonymousWorkflow)
    val workflow = addWorkflow(anonymousWorkflow)
    val order = FreshOrder(orderIdIterator.next(), workflow.path, arguments = orderArguments)
    controller.runOrder(order).map(_.value)

  private def addWorkflow(anonymousWorkflow: Workflow): Workflow =
    val workflowId = nextWorkflowId()
    val workflow = anonymousWorkflow.withId(workflowId)
    directoryProvider.updateVersionedItems(controller, workflowId.versionId, Seq(workflow))
    workflow

  private def nextWorkflowId(): WorkflowId =
    workflowPathIterator.next() ~ versionIdIterator.next()

  private def testPrintAndParse(anonymousWorkflow: Workflow): Unit =
    val workflowNotation = WorkflowPrinter.print(anonymousWorkflow.withoutSource)
    val reparsedWorkflow = WorkflowParser.parse(workflowNotation).map(_.withoutSource)
    logger.debug(workflowNotation)
    assert(reparsedWorkflow == Right(anonymousWorkflow.withoutSource))

  private def agentProcessLimit: Option[Int] =
    controllerState.keyToItem(AgentRef)(agentPath).processLimit


object ExecuteTest:

  private val logger = Logger[this.type]
  private val agentPath = AgentPath("AGENT")

  private def returnCodeScript(returnCode: Int) =
    if isWindows then s"@exit $returnCode"
    else s"exit $returnCode"

  private def returnCodeScript(envName: String) =
    if isWindows then s"@exit %$envName%"
    else s"""exit "$$$envName""""

  private val jobResource = JobResource(JobResourcePath("JOB-RESOURCE"),
    variables = Map(
      "VARIABLE" -> StringConstant("JOB-RESOURCE-VARIABLE-VALUE")))

  private final class TestInternalJob extends InternalJob:
    def toOrderProcess(step: Step) =
      OrderProcess:
        IO:
          OrderOutcome.Completed.fromChecked:
            step.arguments.checked("ARG").flatMap(_.asNumber).map: number =>
              OrderOutcome.Succeeded(NamedValues("RESULT" -> NumberValue(number + 1)))

  private object TestInternalJob extends InternalJob.Companion[TestInternalJob]

  private final class ReturnArgumentsInternalJob extends InternalJob
  :
    def toOrderProcess(step: Step) =
      OrderProcess.succeeded(step.arguments)
  private object ReturnArgumentsInternalJob
  extends InternalJob.Companion[ReturnArgumentsInternalJob]

  private final class ParallelInternalJob extends SemaphoreJob(ParallelInternalJob)
  private object ParallelInternalJob extends SemaphoreJob.Companion[ParallelInternalJob]
