package js7.taskserver.task.process

import java.nio.file.Files._
import js7.agent.data.{AgentTaskId, ProcessKillScript}
import js7.base.process.ProcessSignal.{SIGKILL, SIGTERM}
import js7.base.time.ScalaTime._
import js7.base.utils.Closer.withCloser
import js7.common.process.Processes.newTemporaryShellFile
import js7.common.scalautil.FileUtils.autoDeleting
import js7.common.scalautil.FileUtils.syntax.RichPath
import js7.common.scalautil.Futures.implicits.SuccessFuture
import js7.common.scalautil.IOExecutor.Implicits.globalIOX
import js7.common.system.FileUtils._
import js7.common.system.OperatingSystem.{KernelSupportsNestedShebang, isMac, isSolaris, isUnix, isWindows}
import js7.common.time.WaitForCondition.waitForCondition
import js7.data.job.ReturnCode
import js7.data.system.Stdout
import js7.taskserver.task.process.ShellScriptProcess.startShellScript
import org.scalatest.freespec.AnyFreeSpec
import scala.concurrent.ExecutionContext.Implicits.global

/**
 * @author Joacim Zschimmer
 */
final class ShellScriptProcessTest extends AnyFreeSpec
{
  "ShellScriptProcess" in {
    val envName = "ENVNAME"
    val envValue = "ENVVALUE"
    val exitCode = 42
    val processConfig = ProcessConfiguration.forTest.copy(additionalEnvironment = Map(envName -> envValue))
    val shellProcess = startShellScript(processConfig, name = "TEST", (if (isWindows) "@" else "") + s"exit $exitCode")
    val returnCode = shellProcess.terminated await 99.s
    assert(returnCode == ReturnCode(exitCode))
    assert(!shellProcess.closed.isCompleted)
    shellProcess.close()
    assert(shellProcess.closed.isCompleted)
    shellProcess.terminated await 5.s
    assert(!exists(shellProcess.temporaryScriptFile))
  }

  if (isUnix) {
    if (!KernelSupportsNestedShebang)
      "#! (shebang) not testable because the kernel likely does not support nested interpreters" in {
        pending
      }
    else
      "#! (shebang) is respected" in {
        autoDeleting(newTemporaryShellFile("test-interpreter-")) { interpreter =>
          interpreter :=
            """#! /bin/sh
              |echo INTERPRETER-START
              |sh "$@"
              |echo INTERPRETER-END
              |""".stripMargin
          val scriptString =
            s"""#! $interpreter
               |echo TEST-SCRIPT
               |""".stripMargin
          val stdFileMap = RichProcess.createStdFiles(temporaryDirectory, id = s"ShellScriptProcessTest-shebang")
          val processConfig = ProcessConfiguration.forTest.copy(stdFileMap)
          val shellProcess = startShellScript(processConfig, name = "TEST", scriptString = scriptString)
          shellProcess.terminated await 99.s
          shellProcess.close()
          assert(stdFileMap(Stdout).contentString ==
             """INTERPRETER-START
              |TEST-SCRIPT
              |INTERPRETER-END
              |""".stripMargin)
          RichProcess.tryDeleteFiles(stdFileMap.values)
        }
      }
  }

  "sendProcessSignal SIGKILL" in {
    if (isMac) {
      info("Disabled on MacOS because it kills our builder process")
      pending
    } else {
      val agentTaskId = AgentTaskId("TEST-PROCESS-ID")
      val script = if (isWindows) "echo SCRIPT-ARGUMENTS=%*\nping -n 7 127.0.0.1" else "echo SCRIPT-ARGUMENTS=$*; sleep 6"
      withCloser { closer =>
        val stdFileMap = RichProcess.createStdFiles(temporaryDirectory, id = "ShellScriptProcessTest-kill")
        val killScriptOutputFile = createTempFile("test-", ".tmp")
        val killScriptFile = newTemporaryShellFile("TEST-KILL-SCRIPT")
        killScriptFile := (if (isWindows) s"@echo KILL-ARGUMENTS=%* >$killScriptOutputFile\n" else s"echo KILL-ARGUMENTS=$$* >$killScriptOutputFile\n")
        closer.onClose {
          waitForCondition(15.s, 1.s) {
            RichProcess.tryDeleteFiles(stdFileMap.values)
          }
          delete(killScriptOutputFile)
          delete(killScriptFile)
        }
        val processConfig = ProcessConfiguration.forTest.copy(
          stdFileMap = stdFileMap,
          agentTaskIdOption = Some(agentTaskId),
          killScriptOption = Some(ProcessKillScript(killScriptFile)))
        val shellProcess = startShellScript(processConfig, scriptString = script)
        assert(shellProcess.processConfiguration.files.size == 2)
        sleep(3.s)
        assert(shellProcess.isAlive)
        shellProcess.sendProcessSignal(SIGKILL)
        waitForCondition(10.s, 100.ms) { !shellProcess.isAlive }
        assert(!shellProcess.isAlive)
        val rc = shellProcess.terminated await 99.s
        assert(rc == (
          if (isWindows) ReturnCode(1/* This is Java destroy()*/)
          else if (isSolaris) ReturnCode(SIGKILL.value)  // Solaris: No difference between exit 9 and kill !!!
          else ReturnCode(SIGKILL)))
        shellProcess.close()

        assert(stdFileMap(Stdout).contentString contains "SCRIPT-ARGUMENTS=")
        assert(stdFileMap(Stdout).contentString contains s"SCRIPT-ARGUMENTS=-agent-task-id=${agentTaskId.string}")
        assert(killScriptOutputFile.contentString contains s"KILL-ARGUMENTS=-kill-agent-task-id=${agentTaskId.string}")
      }
    }
  }

  if (!isWindows) {
    "sendProcessSignal SIGTERM (Unix only)" in {
      if (isMac) {
        info("Disabled on MacOS because it kills our builder process")
        pending
      } else {
        val script = "trap 'exit 7' SIGTERM; sleep 1; sleep 1; sleep 1;sleep 1; sleep 1; sleep 1;sleep 1; sleep 1; sleep 1; exit 3"
        val shellProcess = startShellScript(ProcessConfiguration.forTest, scriptString = script)
        sleep(3.s)
        assert(shellProcess.isAlive)
        shellProcess.sendProcessSignal(SIGTERM)
        waitForCondition(10.s, 100.ms) { !shellProcess.isAlive }
        assert(!shellProcess.isAlive)
        val rc = shellProcess.terminated await 99.s
        assert(rc == ReturnCode(7))
        shellProcess.close()
      }
    }
  }
}
