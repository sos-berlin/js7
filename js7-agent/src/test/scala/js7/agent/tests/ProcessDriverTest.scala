package js7.agent.tests

import com.google.inject.Guice
import java.nio.file.Files.createTempDirectory
import js7.agent.configuration.AgentConfiguration
import js7.agent.configuration.inject.AgentModule
import js7.agent.tests.ProcessDriverTest.TestScript
import js7.base.io.file.FileUtils.deleteDirectoryRecursively
import js7.base.io.file.FileUtils.syntax.RichPath
import js7.base.io.process.Processes.{ShellFileExtension => sh}
import js7.base.io.process.ReturnCode
import js7.base.system.OperatingSystem.isWindows
import js7.base.thread.Futures.implicits.SuccessFuture
import js7.base.thread.MonixBlocking.syntax.RichTask
import js7.base.time.ScalaTime._
import js7.base.time.Stopwatch.measureTime
import js7.base.utils.Closer
import js7.common.guice.GuiceImplicits.RichInjector
import js7.data.job.{CommandLine, JobKey, RelativePathExecutable}
import js7.data.order.{HistoricOutcome, Order, OrderId, Outcome}
import js7.data.value.{NamedValues, NumberValue, StringValue}
import js7.data.workflow.WorkflowPath
import js7.data.workflow.position.Position
import js7.executor.StdObservers
import js7.executor.configuration.{JobExecutorConf, TaskConfiguration}
import js7.executor.process.{ProcessDriver, RichProcess}
import monix.execution.Scheduler.Implicits.global
import monix.reactive.subjects.PublishSubject
import org.scalatest.BeforeAndAfterAll
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class ProcessDriverTest extends AnyFreeSpec with BeforeAndAfterAll with TestAgentDirectoryProvider
{
  private lazy val injector = Guice.createInjector(new AgentModule(
    AgentConfiguration.forTest(agentDirectory, name = "ProcessDriverTest")))

  override protected def afterAll() = {
    injector.instance[Closer].close()
    closer.close()
    super.afterAll()
  }

  "ProcessDriver" in {
    val executableDirectory = createTempDirectory("ProcessDriverTest-")

    val pathExecutable = RelativePathExecutable(s"TEST$sh", v1Compatible = true)
    val shellFile = pathExecutable.toFile(executableDirectory)
    shellFile.writeExecutable(TestScript)
    def toOutcome(namedValues: NamedValues, returnCode: ReturnCode) =
      Outcome.Succeeded(namedValues + ("returnCode" -> NumberValue(returnCode.number)))
    val taskConfiguration = TaskConfiguration(JobKey.forTest, toOutcome, CommandLine.fromFile(shellFile))
    info(measureTime(10, "TaskRunner") {
      val order = Order(
        OrderId("TEST"),
        WorkflowPath("JOBCHAIN") ~ "VERSION",
        Order.Processing,
        historicOutcomes = Vector(HistoricOutcome(Position(999), Outcome.Succeeded(Map("a" -> StringValue("A"))))))
      val taskRunner = new ProcessDriver(order.id, taskConfiguration,
        injector.instance[JobExecutorConf])
      val out, err = PublishSubject[String]()
      val stdObservers = new StdObservers(out, err, charBufferSize = 7, keepLastErrLine = false)
      val whenOut = out.foldL.runToFuture
      val whenErr = err.foldL.runToFuture
      val ended = taskRunner.startAndRunProcess(Map("VAR1" -> "VALUE1"), stdObservers)
        .await(30.s).join.await(30.s)
      assert(ended == Outcome.Succeeded(Map(
        "result" -> StringValue("TEST-RESULT-VALUE1"),
        "returnCode" -> NumberValue(0))))
      val nl = System.lineSeparator
      assert(whenOut.await(99.s) == s"Hej!${nl}var1=VALUE1$nl" &&
             whenErr.await(99.s) == s"THIS IS STDERR$nl")
    }.toString)
    RichProcess.tryDeleteFiles(shellFile :: Nil)
    deleteDirectoryRecursively(executableDirectory)
  }
}

object ProcessDriverTest {
  private val TestScript =
    if (isWindows) """
      |@echo off
      |echo Hej!
      |echo THIS IS STDERR >&2
      |echo var1=%VAR1%
      |echo result=TEST-RESULT-%VAR1% >>"%JS7_RETURN_VALUES%"
      |""".stripMargin
    else """
      |echo "Hej!"
      |echo THIS IS STDERR >&2
      |echo "var1=$VAR1"
      |echo "result=TEST-RESULT-$VAR1" >>"$JS7_RETURN_VALUES"
      |""".stripMargin

}
