package js7.launcher.internal

import cats.effect.IO
import cats.effect.unsafe.IORuntime
import java.nio.charset.StandardCharsets.UTF_8
import js7.base.catsutils.CatsEffectExtensions.joinStd
import js7.base.io.process.{Stderr, Stdout}
import js7.base.problem.Checked.*
import js7.base.test.OurAsyncTestSuite
import js7.base.thread.IOExecutor.Implicits.globalIOX
import js7.base.time.AlarmClock
import js7.base.time.ScalaTime.*
import js7.base.utils.ScalaUtils.syntax.RichPartialFunction
import js7.data.agent.AgentPath
import js7.data.controller.ControllerId
import js7.data.job.{InternalExecutable, JobConf, JobKey}
import js7.data.order.{Order, OrderId, OrderOutcome}
import js7.data.subagent.SubagentId
import js7.data.value.expression.Expression.{NamedValue, NumericConstant}
import js7.data.value.expression.Scope
import js7.data.value.{NamedValues, NumberValue}
import js7.data.workflow.instructions.executable.WorkflowJob
import js7.data.workflow.position.{Position, WorkflowBranchPath}
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.launcher.StdObserversForTest.testSink
import js7.launcher.internal.InternalJobLauncherTest.*
import js7.launcher.{OrderProcess, ProcessOrder, StdObservers}

final class InternalJobLauncherTest extends OurAsyncTestSuite:

  private given IORuntime = ioRuntime

  override protected val withIOExecutor = true
  
  "InternalJobLauncher" in:
    val executable = InternalExecutable(
      classOf[TestInternalJob].getName,
      arguments = Map("ARG" -> NamedValue("ARG")))
    val workflowJob = WorkflowJob(AgentPath("AGENT"), executable)
    val executor = new InternalJobLauncher(
      executable,
      JobConf(
        JobKey(WorkflowBranchPath(WorkflowPath("WORKFLOW") ~ "1", Nil), WorkflowJob.Name("JOB")),
        workflowJob,
        workflow,
        ControllerId("CONTROLLER"),
        sigkillDelay = 0.s,
        UTF_8),
      Map.empty,
      blockingJobEC = globalIOX.executionContext,
      null: AlarmClock)

    StdObservers
      .testSink(name = "InternalJobLauncherTest")
      .use: testSink =>
        val orderId = OrderId("TEST")
        val jobKey = JobKey.Named(
          WorkflowBranchPath(WorkflowPath("WORKFLOW"), Nil),
          WorkflowJob.Name("TEST-JOB"))
        for
          orderProcess <- executor
            .toOrderProcess:
              ProcessOrder(
                Order(orderId, workflow.id /: Position(0), Order.Processing(SubagentId("SUBAGENT"))),
                workflow,
                jobKey,
                workflowJob,
                jobResources = Nil,
                executeArguments = Map.empty,
                jobArguments = Map("ARG" -> NumericConstant(1)),
                ControllerId("CONTROLLER"),
                testSink.stdObservers,
                fileValueScope = Scope.empty)
            .map(_.orThrow)
          fiber <- orderProcess.start(orderId, jobKey)
          orderOutcome <- fiber.joinStd
          _ = assert:
            orderOutcome == OrderOutcome.Succeeded(NamedValues("RESULT" -> NumberValue(2)))
          outString <- testSink.out
          errString <- testSink.err
        yield assert:
          outString == "OUT 1/" + "OUT 2" &&
          errString == "ERR 1/" + "ERR 2"


object InternalJobLauncherTest:
  private val workflow = Workflow(WorkflowPath("WORKFLOW") ~ "1", Vector.empty)

  private class TestInternalJob extends InternalJob:
    override def toOrderProcess(step: Step) =
      OrderProcess:
        step.write(Stdout, "OUT 1/")
          *> step.write(Stderr, "ERR 1/")
          *> step.write(Stdout, "OUT 2")
          *> step.write(Stderr, "ERR 2")
          *> IO:
          OrderOutcome.Completed.fromChecked(
            step.arguments.checked("ARG")
              .flatMap(_.as[NumberValue])
              .map(_.number + 1)
              .map(result => OrderOutcome.Succeeded(NamedValues("RESULT" -> NumberValue(result)))))
