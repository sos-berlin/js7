package js7.agent.tests

import cats.syntax.option.*
import java.nio.charset.StandardCharsets.US_ASCII
import java.nio.file.Files.createTempDirectory
import js7.agent.tests.ProcessDriverTest.TestScript
import js7.base.io.file.FileUtils.deleteDirectoryRecursively
import js7.base.io.file.FileUtils.syntax.RichPath
import js7.base.io.process.Processes.ShellFileExtension as sh
import js7.base.io.process.ReturnCode
import js7.base.system.OperatingSystem.isWindows
import js7.base.test.{OurTestSuite, TestCatsEffect}
import js7.base.thread.IOExecutor
import js7.base.thread.CatsBlocking.syntax.*
import js7.base.time.AlarmClock
import js7.base.time.ScalaTime.*
import js7.base.time.Stopwatch.measureTime
import js7.base.utils.CatsUtils.syntax.RichResource
import js7.base.utils.ScalaUtils.syntax.RichEither
import js7.data.job.{CommandLine, JobKey, RelativePathExecutable}
import js7.data.order.{HistoricOutcome, Order, OrderId, OrderOutcome}
import js7.data.subagent.SubagentId
import js7.data.value.{NamedValues, NumberValue, StringValue}
import js7.data.workflow.WorkflowPath
import js7.data.workflow.position.Position
import js7.launcher.StdObservers
import js7.launcher.configuration.{JobLauncherConf, TaskConfiguration}
import js7.launcher.process.{ProcessDriver, RichProcess}
import js7.subagent.configuration.SubagentConf
import cats.effect.IO
import cats.effect.unsafe.{IORuntime, Scheduler}
import js7.base.catsutils.CatsEffectExtensions.joinStd
import org.scalatest.BeforeAndAfterAll

/**
  * @author Joacim Zschimmer
  */
final class ProcessDriverTest
  extends OurTestSuite, BeforeAndAfterAll, TestCatsEffect, TestAgentDirectoryProvider:

  private lazy val ioxAllocated = IOExecutor.resource[IO](SubagentConf.DefaultConfig, "ProcessDriverTest")
    .toAllocated.await(99.s)

  private given IORuntime = ioRuntime
  private given Scheduler = ioRuntime.scheduler

  override protected def afterAll() =
    closer.close()
    ioxAllocated.release.await(99.s)
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

      val jobLauncherConf = JobLauncherConf.checked(
        executablesDirectory = executableDirectory,
        shellScriptTmpDirectory = executableDirectory,
        workTmpDirectory = executableDirectory,
        jobWorkingDirectory = executableDirectory,
        systemEncoding = US_ASCII,
        killScript = None,
        scriptInjectionAllowed = false,
        ioxAllocated.allocatedThing,
        blockingJobEC = ioRuntime.compute/*??? use blocking clause!*/,
        AlarmClock(),
        SubagentConf.DefaultConfig
      ).orThrow

      val io =
        for
          stdObservers <- StdObservers.resource(charBufferSize = 7, keepLastErrLine = false)
          taskRunner = new ProcessDriver(order.id, taskConfiguration, jobLauncherConf)
        yield for
          outFiber <- stdObservers.outStream.compile.foldMonoid.start
          errFiber <- stdObservers.errStream.compile.foldMonoid.start
          outcome <- taskRunner.startAndRunProcess(Map("VAR1" -> "VALUE1".some), stdObservers)
            .flatMap(_.joinStd)
          outString <- outFiber.joinStd
          errString <- errFiber.joinStd
        yield
          assert(outcome == OrderOutcome.Succeeded(Map(
            "result" -> StringValue("TEST-RESULT-VALUE1"),
            "returnCode" -> NumberValue(0))))
          val nl = System.lineSeparator
          assert(outString == s"Hej!${nl}var1=VALUE1$nl" &&
                 errString == s"THIS IS STDERR$nl")
      io.use_.await(99.s)
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
