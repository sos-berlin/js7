package js7.launcher.process

import cats.effect.{IO, Resource}
import fs2.Stream
import java.io.{BufferedOutputStream, FileOutputStream, OutputStreamWriter}
import java.nio.file.Files.*
import java.nio.file.Path
import js7.base.io.file.FileUtils.syntax.RichPath
import js7.base.io.file.FileUtils.temporaryDirectory
import js7.base.io.process.ProcessSignal.{SIGKILL, SIGTERM}
import js7.base.io.process.Processes.newTemporaryShellFile
import js7.base.io.process.{ReturnCode, Stderr, Stdout, StdoutOrStderr}
import js7.base.system.OperatingSystem.{isMac, isSolaris, isUnix, isWindows}
import js7.base.system.ServerOperatingSystem.KernelSupportsNestedShebang
import js7.base.test.OurAsyncTestSuite
import js7.base.thread.CatsBlocking.syntax.*
import js7.base.thread.IOExecutor.Implicits.globalIOX
import js7.base.time.ScalaTime.*
import js7.base.time.WaitForCondition.waitForCondition
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

  "ShellScriptProcess" in:
    val envName = "ENVNAME"
    val envValue = "ENVVALUE"
    val exitCode = 42
    val processConfig = ProcessConfiguration.forTest.copy(additionalEnvironment = Map(envName -> Some(envValue)))
    withScriptFile { scriptFile =>
      scriptFile.writeUtf8Executable((isWindows ?? "@") + s"exit $exitCode")
      val shellProcess = startShellScript(processConfig, scriptFile, Map.empty)
        .await(99.s)
      val returnCode = shellProcess.terminated await 99.s
      assert(returnCode == ReturnCode(exitCode))
      shellProcess.terminated await 5.s
      succeed
    }

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
            val stdFileMap = RichProcess.createStdFiles(temporaryDirectory, id = s"ShellScriptProcessTest-shebang")
            val shellProcess = startShellScript(ProcessConfiguration.forTest, scriptFile, stdFileMap)
              .await(99.s)
            shellProcess.terminated await 99.s
            assert(stdFileMap(Stdout).contentString ==
              """INTERPRETER-START
                |TEST-SCRIPT
                |INTERPRETER-END
                |""".stripMargin)
            RichProcess.tryDeleteFiles(stdFileMap.values)
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
          val stdFileMap = RichProcess.createStdFiles(temporaryDirectory, id = "ShellScriptProcessTest-kill")
          val killScriptOutputFile = createTempFile("test-", ".tmp")
          val killScriptFile = newTemporaryShellFile("TEST-KILL-SCRIPT")
          killScriptFile := (
            if isWindows then
              s"@echo KILL-ARGUMENTS=%* >$killScriptOutputFile\n"
            else
              s"echo KILL-ARGUMENTS=$$* >$killScriptOutputFile\n")
          closer.onClose:
            waitForCondition(15.s, 1.s):
              RichProcess.tryDeleteFiles(stdFileMap.values)
            delete(killScriptOutputFile)
            delete(killScriptFile)
          val processConfig = ProcessConfiguration.forTest.copy(
            maybeTaskId = Some(taskId),
            killScriptOption = Some(ProcessKillScript(killScriptFile)))
          val shellProcess = startShellScript(processConfig, scriptFile, stdFileMap).await(99.s)
          sleep(3.s)
          assert(shellProcess.isAlive)
          shellProcess.sendProcessSignal(SIGKILL).await(99.s)
          awaitAndAssert { !shellProcess.isAlive }
          val rc = shellProcess.terminated await 99.s
          assert(rc == (
            if isWindows then ReturnCode(1/* This is Java destroy()*/)
            else if isSolaris then ReturnCode(SIGKILL.number)  // Solaris: No difference between exit 9 and kill !!!
            else ReturnCode(SIGKILL)))

          assert(stdFileMap(Stdout).contentString contains "SCRIPT-ARGUMENTS=")
          assert(stdFileMap(Stdout).contentString contains s"SCRIPT-ARGUMENTS=--agent-task-id=${taskId.string}")
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
          val shellProcess = startShellScript(ProcessConfiguration.forTest, scriptFile)
            .await(99.s)
          sleep(3.s)
          assert(shellProcess.isAlive)
          shellProcess.sendProcessSignal(SIGTERM).await(99.s)
          awaitAndAssert { !shellProcess.isAlive }
          val rc = shellProcess.terminated await 99.s
          assert(rc == ReturnCode(7))
        }

  private def startShellScript(
    processConfiguration: ProcessConfiguration,
    executable: Path,
    stdFileMap: Map[StdoutOrStderr, Path] = Map.empty)
  : IO[ShellScriptProcess] =
    IO.defer:
      StdObservers
        .resource(charBufferSize = 4096, keepLastErrLine = false)
        .use: stdObservers =>
          def processOutErr(stream: Stream[IO, String], outerr: StdoutOrStderr): Future[Unit] =
            stdFileMap.get(outerr)
              .fold(stream)(file =>
                Stream
                  .resource(Resource.fromAutoCloseable(IO(
                    new OutputStreamWriter(
                      new BufferedOutputStream(
                        new FileOutputStream(file.toFile))))))
                  .flatMap(writer => stream
                    .evalTap(string => IO(writer.write(string)))))
              .compile.drain
              .unsafeToFuture()

          val outErrFut = Future.sequence(Seq(
            processOutErr(stdObservers.outStream, Stdout),
            processOutErr(stdObservers.errStream, Stderr)))

          startPipedShellScript(
            CommandLine(executable.toString :: Nil),
            processConfiguration,
            stdObservers,
            whenTerminated = IO.fromFuture(IO(outErrFut)).void
          ).map(_.orThrow)

  private def withScriptFile[A](body: Path => A): A =
    val file = newTemporaryShellFile("ShellScriptProcessTest")
    try body(file)
    finally tryDeleteFile(file)
