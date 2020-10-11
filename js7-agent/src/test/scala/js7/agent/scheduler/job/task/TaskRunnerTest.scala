package js7.agent.scheduler.job.task

import com.google.inject.Guice
import java.io.Writer
import java.nio.file.Files.{createTempDirectory, setPosixFilePermissions}
import java.nio.file.attribute.PosixFilePermissions
import js7.agent.configuration.AgentConfiguration
import js7.agent.configuration.inject.AgentModule
import js7.agent.scheduler.job.ShellReturnValuesProvider
import js7.agent.scheduler.job.task.TaskRunnerTest._
import js7.agent.tests.TestAgentDirectoryProvider
import js7.base.time.ScalaTime._
import js7.base.time.Stopwatch.measureTime
import js7.base.utils.Closer
import js7.common.guice.GuiceImplicits.RichInjector
import js7.common.process.Processes.{ShellFileExtension => sh}
import js7.common.scalautil.FileUtils.deleteDirectoryRecursively
import js7.common.scalautil.FileUtils.syntax.RichPath
import js7.common.scalautil.Futures.implicits._
import js7.common.system.FileUtils.temporaryDirectory
import js7.common.system.OperatingSystem.{isUnix, isWindows}
import js7.data.agent.AgentName
import js7.data.job.{ExecutablePath, JobKey, ReturnCode}
import js7.data.order.{HistoricOutcome, Order, OrderId, Outcome}
import js7.data.workflow.WorkflowPath
import js7.data.workflow.instructions.executable.WorkflowJob
import js7.data.workflow.position.Position
import js7.taskserver.task.process.{RichProcess, StdChannels}
import monix.execution.Scheduler.Implicits.global
import org.scalatest.BeforeAndAfterAll
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class TaskRunnerTest extends AnyFreeSpec with BeforeAndAfterAll with TestAgentDirectoryProvider {

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
    val taskConfiguration = TaskConfiguration(JobKey.forTest, WorkflowJob(AgentName("TEST"), executablePath, Map("var1" -> "VALUE1")), shellFile, shellReturnValuesProvider)
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
