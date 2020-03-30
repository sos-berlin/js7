package com.sos.jobscheduler.agent.scheduler.job.task

import com.google.inject.Guice
import com.sos.jobscheduler.agent.configuration.AgentConfiguration
import com.sos.jobscheduler.agent.configuration.inject.AgentModule
import com.sos.jobscheduler.agent.scheduler.job.ShellReturnValuesProvider
import com.sos.jobscheduler.agent.scheduler.job.task.TaskRunnerTest._
import com.sos.jobscheduler.agent.tests.TestAgentDirectoryProvider
import com.sos.jobscheduler.base.time.ScalaTime._
import com.sos.jobscheduler.base.utils.Closer
import com.sos.jobscheduler.common.guice.GuiceImplicits.RichInjector
import com.sos.jobscheduler.common.process.Processes.{ShellFileExtension => sh}
import com.sos.jobscheduler.common.scalautil.FileUtils.deleteDirectoryRecursively
import com.sos.jobscheduler.common.scalautil.FileUtils.implicits.RichPath
import com.sos.jobscheduler.common.scalautil.Futures.implicits._
import com.sos.jobscheduler.common.system.FileUtils.temporaryDirectory
import com.sos.jobscheduler.common.system.OperatingSystem.{isUnix, isWindows}
import com.sos.jobscheduler.common.time.Stopwatch.measureTime
import com.sos.jobscheduler.data.agent.AgentRefPath
import com.sos.jobscheduler.data.job.{ExecutablePath, JobKey, ReturnCode}
import com.sos.jobscheduler.data.order.{HistoricOutcome, Order, OrderId, Outcome}
import com.sos.jobscheduler.data.workflow.WorkflowPath
import com.sos.jobscheduler.data.workflow.instructions.executable.WorkflowJob
import com.sos.jobscheduler.data.workflow.position.Position
import com.sos.jobscheduler.taskserver.task.process.{RichProcess, StdChannels}
import java.io.Writer
import java.nio.file.Files.{createTempDirectory, setPosixFilePermissions}
import java.nio.file.attribute.PosixFilePermissions
import monix.execution.Scheduler.Implicits.global
import org.scalatest.{BeforeAndAfterAll, FreeSpec}

/**
  * @author Joacim Zschimmer
  */
final class TaskRunnerTest extends FreeSpec with BeforeAndAfterAll with TestAgentDirectoryProvider {

  private lazy val injector = Guice.createInjector(new AgentModule(AgentConfiguration.forTest(agentDirectory)))

  override protected def afterAll() = {
    injector.instance[Closer].close()
    closer.close()
    super.afterAll()
  }

  "SimpleShellTaskRunner" in {
    val newTaskRunner = injector.instance[SimpleShellTaskRunner.Factory]
    val executableDirectory = createTempDirectory("TaskRunnerTest-")

    val executablePath = ExecutablePath(s"/TEST$sh")
    val shellFile = executablePath.toFile(executableDirectory)
    shellFile := TestScript
    if (isUnix) setPosixFilePermissions(shellFile, PosixFilePermissions.fromString("rwx------"))
    val shellReturnValuesProvider = new ShellReturnValuesProvider(temporaryDirectory)
    val taskConfiguration = TaskConfiguration(JobKey.forTest, WorkflowJob(AgentRefPath("/TEST"), executablePath, Map("var1" -> "VALUE1")), shellFile, shellReturnValuesProvider)
    info(measureTime(10, "TaskRunner") {
      val order = Order(
        OrderId("TEST"),
        WorkflowPath("/JOBCHAIN") ~ "VERSION",
        Order.Processing,
        historicOutcomes = HistoricOutcome(Position(999), Outcome.Succeeded(Map("a" -> "A"))) :: Nil)
      val taskRunner = newTaskRunner(taskConfiguration)
      val stdoutWriter = new TestStdoutStderrWriter
      val stderrWriter = new TestStdoutStderrWriter
      val stdChannels = new StdChannels(charBufferSize = 10, stdoutWriter = stdoutWriter, stderrWriter = stderrWriter)
      val ended = taskRunner.processOrder(order, Map.empty, stdChannels) andThen { case _ => taskRunner.terminate() } await 30.s
      assert(ended == TaskStepSucceeded(Map("result" -> "TEST-RESULT-VALUE1"), ReturnCode(0)))
      val nl = System.lineSeparator
      assert(stdoutWriter.string == s"Hej!${nl}var1=VALUE1$nl")
      assert(stderrWriter.string == s"THIS IS STDERR$nl")
    }.toString)
    RichProcess.tryDeleteFiles(shellFile :: shellReturnValuesProvider.file :: Nil)
    deleteDirectoryRecursively(executableDirectory)
  }
}

object TaskRunnerTest {
  private val TestScript =
    if (isWindows) """
      |@echo off
      |echo Hej!
      |echo THIS IS STDERR>&2
      |echo var1=%SCHEDULER_PARAM_VAR1%
      |echo result=TEST-RESULT-%SCHEDULER_PARAM_VAR1% >>"%SCHEDULER_RETURN_VALUES%"
      |""".stripMargin
    else """
      |echo "Hej!"
      |echo THIS IS STDERR >&2
      |echo "var1=$SCHEDULER_PARAM_VAR1"
      |echo "result=TEST-RESULT-$SCHEDULER_PARAM_VAR1" >>"$SCHEDULER_RETURN_VALUES"
      |""".stripMargin

  private final class TestStdoutStderrWriter extends Writer {
    private val stringBuilder = new StringBuilder

    def chunkSize = 10

    def write(chars: Array[Char], offset: Int, length: Int) =
      stringBuilder.appendAll(chars, offset, length)

    def string = stringBuilder.toString

    def flush() = {}

    def close() = {}
  }
}
