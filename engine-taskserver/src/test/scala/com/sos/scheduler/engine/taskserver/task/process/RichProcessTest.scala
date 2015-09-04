package com.sos.scheduler.engine.taskserver.task.process

import com.sos.scheduler.engine.base.process.ProcessSignal.{SIGKILL, SIGTERM}
import com.sos.scheduler.engine.common.scalautil.Closers.implicits.RichClosersCloser
import com.sos.scheduler.engine.common.scalautil.Closers.withCloser
import com.sos.scheduler.engine.common.scalautil.FileUtils.implicits.RichPath
import com.sos.scheduler.engine.common.system.OperatingSystem.isWindows
import com.sos.scheduler.engine.common.time.ScalaTime._
import com.sos.scheduler.engine.common.time.WaitForCondition.waitForCondition
import com.sos.scheduler.engine.data.job.ReturnCode
import com.sos.scheduler.engine.taskserver.task.process.RichProcess.startShellScript
import com.sos.scheduler.engine.taskserver.task.process.StdoutStderr.Stdout
import java.nio.file.Files
import java.nio.file.Files.delete
import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.junit.JUnitRunner

/**
 * @author Joacim Zschimmer
 */
@RunWith(classOf[JUnitRunner])
final class RichProcessTest extends FreeSpec {

  "RichProcess" in {
    val envName = "ENVNAME"
    val envValue = "ENVVALUE"
    val exitCode = 42
    val processConfig = ProcessConfiguration(additionalEnvironment = Map(envName → envValue))
    val shellProcess = RichProcess.startShellScript(processConfig, name = "TEST", s"exit $exitCode")
    val returnCode = shellProcess.waitForTermination()
    assert(returnCode == ReturnCode(exitCode))
    assert(!shellProcess.closed.isCompleted)
    shellProcess.close()
    assert(shellProcess.closed.isCompleted)
  }

  "sendProcessSignal SIGKILL" in {
    val idString = "TEST-PROCESS-ID"
    val script = if (isWindows) "echo ARGUMENTS=%*\nping -n 60 127.0.0.1" else "echo ARGUMENTS=$*; sleep 60"
    withCloser { closer ⇒
      val stdFileMap = RichProcess.createTemporaryStdFiles()
      val testFile = Files.createTempFile("test-", ".tmp")
      val killScriptFile = RichProcess.OS.newTemporaryShellFile("TEST")
      killScriptFile.contentString = if (isWindows) s"echo KILL-ARGUMENTS=%* >$testFile\n" else s"echo KILL-ARGUMENTS=$$* >$testFile\n"
      closer.onClose {
        RichProcess.tryDeleteFiles(stdFileMap.values)  // Under Windows the files will not be deleted, because ping.exe will be not killed (and JS-1468 doesn't allow integrated kill scripts) !!!
        delete(testFile)
        delete(killScriptFile)
      }
      val processConfig = ProcessConfiguration(stdFileMap = stdFileMap, idStringOption = Some(idString), killScriptFileOption = Some(killScriptFile))
      val shellProcess = startShellScript(processConfig, scriptString = script)
      assert(shellProcess.processConfiguration.files.size == 3)
      sleep(3.s)
      assert(shellProcess.isAlive)
      shellProcess.sendProcessSignal(SIGKILL)
      waitForCondition(10.s, 100.ms) { !shellProcess.isAlive }
      assert(!shellProcess.isAlive)
      val rc = shellProcess.waitForTermination()
      assert(rc == (if (isWindows) ReturnCode(1/* This is Java destroy()*/) else ReturnCode(SIGKILL)))
      shellProcess.close()
      assert(stdFileMap(Stdout).contentString contains "ARGUMENTS=")
      assert(stdFileMap(Stdout).contentString contains s"ARGUMENTS=-agent-task-id=$idString")
      assert(testFile.contentString contains s"KILL-ARGUMENTS=-kill-agent-task-id=$idString")
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
