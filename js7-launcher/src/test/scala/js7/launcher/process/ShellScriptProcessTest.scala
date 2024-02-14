package js7.launcher.process

import cats.effect.IO
import cats.effect.unsafe.IORuntime
import java.nio.file.Files.*
import java.nio.file.Path
import js7.base.io.file.FileUtils.syntax.RichPath
import js7.base.io.process.ProcessSignal.{SIGKILL, SIGTERM}
import js7.base.io.process.Processes.{newLogFile, newTemporaryShellFile, temporaryShellFileResource}
import js7.base.io.process.{Processes, ReturnCode, StdoutOrStderr}
import js7.base.system.OperatingSystem.{isMac, isSolaris, isUnix, isWindows}
import js7.base.system.ServerOperatingSystem.KernelSupportsNestedShebang
import js7.base.test.OurAsyncTestSuite
import js7.base.thread.CatsBlocking.syntax.*
import js7.base.thread.IOExecutor.Implicits.globalIOX
import js7.base.time.ScalaTime.*
import js7.base.utils.Closer.withCloser
import js7.base.utils.ScalaUtils.syntax.*
import js7.data.job.{CommandLine, TaskId}
import js7.launcher.StdObservers
import js7.launcher.configuration.ProcessKillScript
import js7.launcher.process.RichProcess.tryDeleteFile
import js7.launcher.process.ShellScriptProcess.startPipedShellScript
import js7.tester.ScalaTestUtils.awaitAndAssert
import scala.concurrent.Future

/**
 * @author Joacim Zschimmer
 */
final class ShellScriptProcessTest extends OurAsyncTestSuite:

  private given IORuntime = ioRuntime

  "ShellScriptProcess" in:
    val envName = "ENVNAME"
    val envValue = "ENVVALUE"
    val exitCode = 42
    val processConfig = ProcessConfiguration.forTest.copy(additionalEnvironment = Map(envName -> Some(envValue)))
    withScriptFile { scriptFile =>
      scriptFile.writeUtf8Executable((isWindows ?? "@") + s"exit $exitCode")
      val (shellProcess, sink) = startShellScript(processConfig, scriptFile)
        .await(99.s)
      val returnCode = shellProcess.terminated await 99.s
      assert(returnCode == ReturnCode(exitCode))
      shellProcess.terminated await 5.s
      succeed
    }

  "stdout" in:
    temporaryShellFileResource[IO]("stdout").use: scriptFile =>
      for
        _ <- IO(scriptFile.writeUtf8Executable:
          (isWindows ?? "@echo off\n") +
            """echo TEST-SCRIPT-1
              |echo TEST-SCRIPT-2""".stripMargin)
        pair <- startShellScript(ProcessConfiguration.forTest, scriptFile)
        (shellProcess, sink) = pair
        rc <- shellProcess.terminated
      yield
        assert(rc == ReturnCode(0) &&
          sink.out.await(99.s) == "TEST-SCRIPT-1\nTEST-SCRIPT-2\n")
        succeed

  if isUnix then
    if !KernelSupportsNestedShebang then
      "#! (shebang) not testable because the kernel likely does not support nested interpreters" in:
        pending
    else
      "#! (shebang) is respected" in:
        withScriptFile { interpreter =>
          interpreter :=
            """#! /bin/sh
              |echo INTERPRETER-START
              |sh "$@"
              |echo INTERPRETER-END
              |""".stripMargin
          withScriptFile { scriptFile =>
            scriptFile.writeUtf8Executable(
              s"""#! $interpreter
                 |echo TEST-SCRIPT
                 |""".stripMargin)
            val (shellProcess, sink) = startShellScript(ProcessConfiguration.forTest, scriptFile)
              .await(99.s)
            shellProcess.terminated await 99.s
            assert(sink.out.await(99.s) ==
              """INTERPRETER-START
                |TEST-SCRIPT
                |INTERPRETER-END
                |""".stripMargin)
            succeed
          }
        }

  "sendProcessSignal SIGKILL" in:
    if isMac then
      info("Disabled on MacOS because it kills our builder process")
      pending
    else
      val taskId = TaskId("TEST-PROCESS-ID")
      withScriptFile { scriptFile =>
        scriptFile.writeUtf8Executable(
          if isWindows then
            """echo SCRIPT-ARGUMENTS=%*
              |ping -n 7 127.0.0.1
              |""".stripMargin
          else
            "echo SCRIPT-ARGUMENTS=$*; sleep 6")
        withCloser { closer =>
          val killScriptOutputFile = createTempFile("test-", ".tmp")
          val killScriptFile = newTemporaryShellFile("TEST-KILL-SCRIPT")
          killScriptFile := (
            if isWindows then
              s"@echo KILL-ARGUMENTS=%* >$killScriptOutputFile\n"
            else
              s"echo KILL-ARGUMENTS=$$* >$killScriptOutputFile\n")
          closer.onClose:
            delete(killScriptOutputFile)
            delete(killScriptFile)
          val processConfig = ProcessConfiguration.forTest.copy(
            maybeTaskId = Some(taskId),
            killScriptOption = Some(ProcessKillScript(killScriptFile)))
          val (shellProcess, sink) = startShellScript(processConfig, scriptFile).await(99.s)
          sleep(3.s)
          assert(shellProcess.isAlive)
          shellProcess.sendProcessSignal(SIGKILL).await(99.s)
          awaitAndAssert { !shellProcess.isAlive }
          val rc = shellProcess.terminated await 99.s
          assert(rc == (
            if isWindows then ReturnCode(1/* This is Java destroy()*/)
            else if isSolaris then ReturnCode(SIGKILL.number)  // Solaris: No difference between exit 9 and kill !!!
            else ReturnCode(SIGKILL)))

          assert(sink.out.await(99.s) contains "SCRIPT-ARGUMENTS=")
          assert(sink.out.await(99.s) contains s"SCRIPT-ARGUMENTS=--agent-task-id=${taskId.string}")
          assert(killScriptOutputFile.contentString contains s"KILL-ARGUMENTS=--kill-agent-task-id=${taskId.string}")
        }
      }

  if !isWindows then
    "sendProcessSignal SIGTERM (Unix only)" in:
      if isMac then
        info("Disabled on MacOS because it kills our builder process")
        pending
      else
        withScriptFile { scriptFile =>
          scriptFile.writeUtf8Executable(
            "trap 'exit 7' SIGTERM; sleep 1; sleep 1; sleep 1;sleep 1; sleep 1; sleep 1;sleep 1; sleep 1; sleep 1; exit 3")
          val (shellProcess, sink) = startShellScript(ProcessConfiguration.forTest, scriptFile)
            .await(99.s)
          sleep(3.s)
          assert(shellProcess.isAlive)
          shellProcess.sendProcessSignal(SIGTERM).await(99.s)
          awaitAndAssert { !shellProcess.isAlive }
          val rc = shellProcess.terminated await 99.s
          assert(rc == ReturnCode(7))
        }

  private def createStdFiles(directory: Path, id: String): Map[StdoutOrStderr, Path] =
    StdoutOrStderr.values
      .map: o =>
        o -> newLogFile(directory, id, o)
      .toMap

  private def startShellScript(
    processConfiguration: ProcessConfiguration,
    executable: Path)
  : IO[(ShellScriptProcess, StdObservers.TestSink)] =
    StdObservers
      .testSink(name = "ShellScriptProcessTest")
      .use: testSink =>
        for
          checkedProcess <- startPipedShellScript(
            CommandLine(List(executable.toString)),
            processConfiguration,
            testSink.stdObservers,
            name = "ShellScriptProcessTest")
        yield
          checkedProcess.orThrow -> testSink

  private def withScriptFile[A](body: Path => A): A =
    val file = newTemporaryShellFile("ShellScriptProcessTest")
    try body(file)
    finally tryDeleteFile(file)
