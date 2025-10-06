package js7.launcher.process

import cats.effect.unsafe.{IORuntime, Scheduler}
import cats.effect.{IO, Resource}
import cats.syntax.option.*
import cats.syntax.traverse.*
import java.nio.charset.StandardCharsets.US_ASCII
import java.nio.file.Files.createTempDirectory
import js7.base.catsutils.Environment.environment
import js7.base.config.Js7Config
import js7.base.configutils.Configs.HoconStringInterpolator
import js7.base.io.file.FileDeleter.tryDeleteFiles
import js7.base.io.file.FileUtils.deleteDirectoryRecursively
import js7.base.io.file.FileUtils.syntax.RichPath
import js7.base.io.process.Processes.ShellFileExtension as sh
import js7.base.io.process.ReturnCode
import js7.base.system.OperatingSystem.isWindows
import js7.base.test.OurAsyncTestSuite
import js7.base.thread.CatsBlocking.syntax.*
import js7.base.thread.IOExecutor
import js7.base.time.ScalaTime.*
import js7.base.time.{AlarmClock, Stopwatch}
import js7.base.utils.ScalaUtils.syntax.RichEither
import js7.base.utils.Tests.isIntelliJIdea
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
import js7.launcher.configuration.JobLauncherConf
import js7.launcher.crashpidfile.CrashPidFileService
import js7.launcher.process.ProcessDriverTest.*
import org.scalatest.BeforeAndAfterAll

/**
  * @author Joacim Zschimmer
  */
final class ProcessDriverTest extends OurAsyncTestSuite, BeforeAndAfterAll:

  private val config = config"""
    js7.thread-pools.long-blocking.keep-alive = 3s
    js7.thread-pools.long-blocking.virtual = true
    js7.job.execution.kill-with-sigterm-command = [ "/bin/kill", "$$pid" ]
    js7.job.execution.kill-with-sigkill-command = [ "/bin/kill", "-KILL", "$$pid" ]
    js7.job.execution.kill-command-for-windows = [ "taskkill", "/pid", "$$pid" ]
    js7.job.execution.used-error-line-length = 4096
    js7.job.execution.worry-about-stdout-after-termination = 100ms
    """.withFallback(Js7Config.defaultConfig)

  private given IORuntime = ioRuntime
  private given Scheduler = ioRuntime.scheduler

  "ProcessDriver" in:
    val executableDirectory = createTempDirectory("ProcessDriverTest-")

    val pathExecutable = RelativePathExecutable(s"TEST$sh", v1Compatible = true)
    val shellFile = pathExecutable.toFile(executableDirectory)
    shellFile.writeUtf8Executable(TestScript)

    def toOutcome(namedValues: NamedValues, returnCode: ReturnCode) =
      OrderOutcome.Succeeded(namedValues + ("returnCode" -> NumberValue(returnCode.number)))
    val conf = ProcessDriver.Conf(JobKey.forTest, toOutcome, CommandLine.fromFile(shellFile))

    val order = Order(
      OrderId("TEST"),
      WorkflowPath("JOBCHAIN") ~ "VERSION" /: Position(0),
      Order.Processing(SubagentId("SUBAGENT")),
      historicOutcomes = Vector:
        HistoricOutcome(Position(999), OrderOutcome.Succeeded(Map("a" -> StringValue("A")))))

    CrashPidFileService.file(executableDirectory / "crashpidfile").use: pidFile =>
      unlimitedExecutionContextResource[IO]("ProcessDriverTest-blocking").use: jobEC =>
        val jobLauncherConf = JobLauncherConf.checked(
          executablesDirectory = executableDirectory,
          shellScriptTmpDirectory = executableDirectory,
          workTmpDirectory = executableDirectory,
          jobWorkingDirectory = executableDirectory,
          systemEncoding = US_ASCII,
          scriptInjectionAllowed = false,
          environment[IOExecutor].await(99.s),
          blockingJobEC = jobEC,
          AlarmClock(),
          pidFile,
          config
        ).orThrow

        val n = if isIntelliJIdea then 100 else 10
        Seq.fill(n)(()).traverse: _ =>
          locally:
            for
              testSink <- StdObservers.testSink(charBufferSize = 7, name = "ProcessDriverTest")
              processDriver = new ProcessDriver(order.id, conf, jobLauncherConf)
              assertion <- Resource.eval:
                for
                  outcome <- processDriver
                    .runProcess(Map("VAR1" -> "VALUE1".some), testSink.stdObservers)
                  out <- testSink.out
                  err <- testSink.err
                yield
                  assert(outcome == OrderOutcome.Succeeded(Map(
                    "result" -> StringValue("TEST-RESULT-VALUE1"),
                    "returnCode" -> NumberValue(0))))
                  val nl = System.lineSeparator
                  assert(out == s"Hej!${nl}var1=VALUE1$nl" &&
                         err == s"THIS IS STDERR$nl")
            yield
              assertion
          .use_
        .timed.flatMap: (duration, _) =>
          IO(info(Stopwatch.itemsPerSecondString(duration, n, "ProcessDriver")))
        .as(succeed)
      .guarantee(IO:
        tryDeleteFiles(shellFile :: Nil)
        deleteDirectoryRecursively(executableDirectory))


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
