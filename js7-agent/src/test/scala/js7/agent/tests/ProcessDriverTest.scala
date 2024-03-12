package js7.agent.tests

import cats.effect.IO
import cats.effect.kernel.Resource
import cats.effect.unsafe.{IORuntime, Scheduler}
import cats.syntax.option.*
import java.nio.charset.StandardCharsets.US_ASCII
import java.nio.file.Files.createTempDirectory
import js7.agent.tests.ProcessDriverTest.TestScript
import js7.base.configutils.Configs.HoconStringInterpolator
import js7.base.io.file.FileUtils.deleteDirectoryRecursively
import js7.base.io.file.FileUtils.syntax.RichPath
import js7.base.io.process.Processes.ShellFileExtension as sh
import js7.base.io.process.ReturnCode
import js7.base.system.OperatingSystem.isWindows
import js7.base.test.{OurTestSuite}
import js7.base.thread.CatsBlocking.syntax.*
import js7.base.thread.IOExecutor
import js7.base.time.AlarmClock
import js7.base.time.ScalaTime.*
import js7.base.time.Stopwatch.measureTime
import js7.base.utils.CatsUtils.syntax.RichResource
import js7.base.utils.ScalaUtils.syntax.RichEither
import js7.common.system.ThreadPools
import js7.common.system.ThreadPools.unlimitedExecutionContextResource
import js7.data.job.{CommandLine, JobKey, RelativePathExecutable}
import js7.data.order.{HistoricOutcome, Order, OrderId, OrderOutcome}
import js7.data.subagent.SubagentId
import js7.data.value.{NamedValues, NumberValue, StringValue}
import js7.data.workflow.WorkflowPath
import js7.data.workflow.position.Position
import js7.launcher.StdObservers
import js7.launcher.StdObserversForTest.testSink
import js7.launcher.configuration.{JobLauncherConf, TaskConfiguration}
import js7.launcher.process.{ProcessDriver, RichProcess}
import js7.subagent.configuration.SubagentConf
import org.scalatest.BeforeAndAfterAll
import org.scalatest.compatible.Assertion

/**
  * @author Joacim Zschimmer
  */
final class ProcessDriverTest
  extends OurTestSuite, BeforeAndAfterAll, TestAgentDirectoryProvider:

  private lazy val ioxAllocated = IOExecutor.resource[IO](SubagentConf.DefaultConfig, "ProcessDriverTest")
    .toAllocated.await(99.s)

  private given IORuntime = ioRuntime
  private given Scheduler = ioRuntime.scheduler

  private val config = config"""
    js7.thread-pools.long-blocking.keep-alive = 3s
    js7.thread-pools.long-blocking.virtual = true
    """

  override protected def afterAll() =
    try
      closer.close()
      ioxAllocated.release.await(99.s)
    finally
      super.afterAll()

  "ProcessDriver" in:
    val executableDirectory = createTempDirectory("ProcessDriverTest-")

    val pathExecutable = RelativePathExecutable(s"TEST$sh", v1Compatible = true)
    val shellFile = pathExecutable.toFile(executableDirectory)
    shellFile.writeUtf8Executable(TestScript)
    def toOutcome(namedValues: NamedValues, returnCode: ReturnCode) =
      OrderOutcome.Succeeded(namedValues + ("returnCode" -> NumberValue(returnCode.number)))
    val taskConfiguration = TaskConfiguration(JobKey.forTest, toOutcome, CommandLine.fromFile(shellFile))
    info(measureTime(10, "TaskRunner") {
      val order = Order(
        OrderId("TEST"),
        WorkflowPath("JOBCHAIN") ~ "VERSION" /: Position(0),
        Order.Processing(SubagentId("SUBAGENT")),
        historicOutcomes = Vector(HistoricOutcome(Position(999), OrderOutcome.Succeeded(Map("a" -> StringValue("A"))))))

      val resource: Resource[IO, Assertion] =
        for
          jobEC <- unlimitedExecutionContextResource[IO]("ProcessDriverTest-blocking", config)
          jobLauncherConf = JobLauncherConf.checked(
            executablesDirectory = executableDirectory,
            shellScriptTmpDirectory = executableDirectory,
            workTmpDirectory = executableDirectory,
            jobWorkingDirectory = executableDirectory,
            systemEncoding = US_ASCII,
            killScript = None,
            scriptInjectionAllowed = false,
            ioxAllocated.allocatedThing,
            blockingJobEC = jobEC,
            AlarmClock(),
            SubagentConf.DefaultConfig
          ).orThrow

          testSink <- StdObservers.testSink(charBufferSize = 7, name = "ProcessDriverTest")
          taskRunner = new ProcessDriver(order.id, taskConfiguration, jobLauncherConf)
          assertion <- Resource.eval:
            for
              outcome <- taskRunner.runProcess(Map("VAR1" -> "VALUE1".some), testSink.stdObservers)
              out <- testSink.out
              err <- testSink.err
            yield
              assert(outcome == OrderOutcome.Succeeded(Map(
                "result" -> StringValue("TEST-RESULT-VALUE1"),
                "returnCode" -> NumberValue(0))))
              val nl = System.lineSeparator
              assert(out == s"Hej!${nl}var1=VALUE1$nl" &&
                     err == s"THIS IS STDERR$nl")
        yield assertion

      resource.use_.await(99.s)
  }.toString)
    RichProcess.tryDeleteFiles(shellFile :: Nil)
    deleteDirectoryRecursively(executableDirectory)


object ProcessDriverTest:
  private val TestScript =
    if isWindows then """
      |@echo off
      |echo Hej!
      |echo THIS IS STDERR>&2
      |echo var1=%VAR1%
      |echo result=TEST-RESULT-%VAR1% >>"%JS7_RETURN_VALUES%"
      |""".stripMargin
    else """
      |echo "Hej!"
      |echo THIS IS STDERR >&2
      |echo "var1=$VAR1"
      |echo "result=TEST-RESULT-$VAR1" >>"$JS7_RETURN_VALUES"
      |""".stripMargin
