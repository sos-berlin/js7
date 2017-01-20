package com.sos.scheduler.engine.taskserver.task.process

import com.sos.scheduler.engine.agent.data.{AgentTaskId, ProcessKillScript}
import com.sos.scheduler.engine.base.process.ProcessSignal.{SIGKILL, SIGTERM}
import com.sos.scheduler.engine.common.process.Processes.newTemporaryShellFile
import com.sos.scheduler.engine.common.process.StdoutStderr.Stdout
import com.sos.scheduler.engine.common.scalautil.Closers.implicits.RichClosersCloser
import com.sos.scheduler.engine.common.scalautil.Closers.withCloser
import com.sos.scheduler.engine.common.scalautil.FileUtils.autoDeleting
import com.sos.scheduler.engine.common.scalautil.FileUtils.implicits.RichPath
import com.sos.scheduler.engine.common.scalautil.Futures.implicits.SuccessFuture
import com.sos.scheduler.engine.common.system.FileUtils._
import com.sos.scheduler.engine.common.system.OperatingSystem.{KernelSupportsNestedShebang, isSolaris, isUnix, isWindows}
import com.sos.scheduler.engine.common.time.ScalaTime._
import com.sos.scheduler.engine.common.time.WaitForCondition.waitForCondition
import com.sos.scheduler.engine.data.job.ReturnCode
import com.sos.scheduler.engine.taskserver.task.process.ShellScriptProcess.startShellScript
import java.nio.file.Files._
import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.junit.JUnitRunner
import scala.concurrent.ExecutionContext.Implicits.global

/**
 * @author Joacim Zschimmer
 */
@RunWith(classOf[JUnitRunner])
final class ShellScriptProcessTest extends FreeSpec {

  "ShellScriptProcess" in {
    val envName = "ENVNAME"
    val envValue = "ENVVALUE"
    val exitCode = 42
    val processConfig = ProcessConfiguration(additionalEnvironment = Map(envName → envValue))
    val shellProcess = startShellScript(processConfig, name = "TEST", s"exit $exitCode")
    val returnCode = shellProcess.waitForTermination()
    assert(returnCode == ReturnCode(exitCode))
    assert(!shellProcess.closed.isCompleted)
    shellProcess.close()
    assert(shellProcess.closed.isCompleted)
    shellProcess.scriptFileDeleted await 5.s
    assert(!exists(shellProcess.temporaryScriptFile))
  }

  if (isUnix) {
    if (!KernelSupportsNestedShebang)
      "#! (shebang) not testable because the kernel likely does not support nested interpreters" in {
        pending
      }
    else
      "#! (shebang) is respected" in {
        autoDeleting(newTemporaryShellFile("test-interpreter-")) { interpreter ⇒
          interpreter.contentString =
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
          val processConfig = ProcessConfiguration(stdFileMap)
          val shellProcess = startShellScript(processConfig, name = "TEST", scriptString = scriptString)
          shellProcess.waitForTermination()
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
    val agentTaskId = AgentTaskId("TEST-PROCESS-ID")
    val script = if (isWindows) "echo SCRIPT-ARGUMENTS=%*\nping -n 7 127.0.0.1" else "echo SCRIPT-ARGUMENTS=$*; sleep 6"
    withCloser { closer ⇒
      val stdFileMap = RichProcess.createStdFiles(temporaryDirectory, id = "ShellScriptProcessTest-kill")
      val killScriptOutputFile = createTempFile("test-", ".tmp")
      val killScriptFile = newTemporaryShellFile("TEST-KILL-SCRIPT")
      killScriptFile.contentString = if (isWindows) s"echo KILL-ARGUMENTS=%* >$killScriptOutputFile\n" else s"echo KILL-ARGUMENTS=$$* >$killScriptOutputFile\n"
      closer.onClose {
        waitForCondition(15.s, 1.s) {
          RichProcess.tryDeleteFiles(stdFileMap.values)
        }
        delete(killScriptOutputFile)
        delete(killScriptFile)
      }
      val processConfig = ProcessConfiguration(
        stdFileMap = stdFileMap,
        agentTaskIdOption = Some(agentTaskId),
        killScriptOption = Some(ProcessKillScript(killScriptFile)))
      val shellProcess = startShellScript(processConfig, scriptString = script)
      assert(shellProcess.processConfiguration.files.size == 3)
      sleep(3.s)
      assert(shellProcess.isAlive)
      shellProcess.sendProcessSignal(SIGKILL)
      waitForCondition(10.s, 100.ms) { !shellProcess.isAlive }
      assert(!shellProcess.isAlive)
      val rc = shellProcess.waitForTermination()
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

  if (!isWindows) {
    "sendProcessSignal SIGTERM (Unix only)" in {
      val script = "trap 'exit 7' SIGTERM; sleep 1; sleep 1; sleep 1;sleep 1; sleep 1; sleep 1;sleep 1; sleep 1; sleep 1; exit 3"
      val shellProcess = startShellScript(ProcessConfiguration(), scriptString = script)
      sleep(3.s)
      assert(shellProcess.isAlive)
      shellProcess.sendProcessSignal(SIGTERM)
      waitForCondition(10.s, 100.ms) { !shellProcess.isAlive }
      assert(!shellProcess.isAlive)
      val rc = shellProcess.waitForTermination()
      assert(rc == ReturnCode(7))
      shellProcess.close()
    }
  }
}
