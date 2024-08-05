package js7.launcher.process

import cats.effect.unsafe.IORuntime
import cats.effect.{IO, ResourceIO}
import java.nio.file.Path
import js7.base.io.file.FileDeleter.tryDeleteFile
import js7.base.io.file.FileUtils.syntax.RichPath
import js7.base.io.process.ProcessSignal.SIGTERM
import js7.base.io.process.Processes.{newTemporaryShellFile, temporaryShellFileResource}
import js7.base.io.process.{Processes, ReturnCode}
import js7.base.system.OperatingSystem.{isUnix, isWindows}
import js7.base.system.ServerOperatingSystem.KernelSupportsNestedShebang
import js7.base.test.OurAsyncTestSuite
import js7.base.thread.CatsBlocking.syntax.*
import js7.base.thread.IOExecutor.Implicits.globalIOX
import js7.base.time.ScalaTime.*
import js7.base.utils.ScalaUtils.syntax.*
import js7.data.job.{CommandLine, JobKey, TaskId}
import js7.data.order.OrderId
import js7.data.workflow.WorkflowPath
import js7.data.workflow.position.Position
import js7.launcher.StdObserversForTest.testSink
import js7.launcher.{StdObservers, StdObserversForTest}
import scala.concurrent.Future

// See also CancelOrdersTest
/**
 * @author Joacim Zschimmer
 */
final class PipedProcessTest extends OurAsyncTestSuite:

  private given IORuntime = ioRuntime

  "PipedProcess" in:
    val envName = "ENVNAME"
    val envValue = "ENVVALUE"
    val exitCode = 42
    val processConfig = ProcessConfiguration.forTest.copy(additionalEnvironment = Map(envName -> Some(envValue)))
    withScriptFile: scriptFile =>
      scriptFile.writeUtf8Executable((isWindows ?? "@") + s"exit $exitCode")
      val (returnCode, sink) = runShellScript(processConfig, scriptFile)
        .await(99.s)
      assert(returnCode == ReturnCode(exitCode))
      succeed

  "stdout" in:
    temporaryShellFileResource[IO]("stdout").use: scriptFile =>
      for
        _ <- IO(scriptFile.writeUtf8Executable:
          (isWindows ?? "@echo off\n") +
            """echo TEST-SCRIPT-1
              |echo TEST-SCRIPT-2""".stripMargin)
        _ <- runningShellScript(ProcessConfiguration.forTest, scriptFile)
          .use: (shellProcess, sink) =>
            for
              rc <- shellProcess.watchProcessAndStdouterr
            yield
              assert(rc == ReturnCode(0) &&
                sink.out.await(99.s) == "TEST-SCRIPT-1\nTEST-SCRIPT-2\n")
      yield succeed

  if isUnix then
    if !KernelSupportsNestedShebang then
      "#! (shebang) not testable because the kernel likely does not support nested interpreters" in:
        pending
    else
      "#! (shebang) is respected" in:
        withScriptFile: interpreter =>
          interpreter :=
            """#! /bin/sh
              |echo INTERPRETER-START
              |sh "$@"
              |echo INTERPRETER-END
              |""".stripMargin
          withScriptFile: scriptFile =>
            scriptFile.writeUtf8Executable(
              s"""#! $interpreter
                 |echo TEST-SCRIPT
                 |""".stripMargin)
            val (returnCode, sink) = runShellScript(ProcessConfiguration.forTest, scriptFile)
              .await(99.s)
            assert(sink.out.await(99.s) ==
              """INTERPRETER-START
                |TEST-SCRIPT
                |INTERPRETER-END
                |""".stripMargin)
            succeed

  if !isWindows then
    "sendProcessSignal SIGTERM (Unix only)" in:
      withScriptFile: scriptFile =>
        scriptFile.writeUtf8Executable:
          "trap 'exit 7' SIGTERM; sleep 1; sleep 1; sleep 1;sleep 1; sleep 1; sleep 1;sleep 1; sleep 1; sleep 1; exit 3"
        runningShellScript(ProcessConfiguration.forTest, scriptFile)
          .use: (shellProcess, sink) =>
            IO:
              sleep(1.s)
              assert(shellProcess.isAlive)
              shellProcess.sendProcessSignal(SIGTERM).await(99.s)
              val rc = shellProcess.watchProcessAndStdouterr await 99.s
              assert(rc == ReturnCode(7))
          .await(99.s)

  private def runShellScript(
    processConfiguration: ProcessConfiguration,
    executable: Path)
  : IO[(ReturnCode, StdObserversForTest.TestSink)] =
    runningShellScript(processConfiguration, executable)
      .use: (richProcess, sink) =>
        richProcess.watchProcessAndStdouterr
          .map(_ -> sink)

  private def runningShellScript(
    processConfiguration: ProcessConfiguration,
    executable: Path)
  : ResourceIO[(PipedProcess, StdObserversForTest.TestSink)] =
    StdObservers
      .testSink(name = "PipedProcessTest")
      .evalMap: testSink =>
        for
          checkedProcess <- PipedProcess.start(
            CommandLine(List(executable.toString)),
            processConfiguration,
            testSink.stdObservers,
            OrderId("ORDER"), JobKey.Anonymous(WorkflowPath("WORKFLOW") /: Position(0)))
        yield
          checkedProcess.orThrow -> testSink

  private def withScriptFile[A](body: Path => A): A =
    val file = newTemporaryShellFile("PipedProcessTest")
    try body(file)
    finally tryDeleteFile(file)
