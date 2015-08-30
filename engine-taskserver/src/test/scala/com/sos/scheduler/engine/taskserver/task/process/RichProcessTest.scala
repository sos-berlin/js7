package com.sos.scheduler.engine.taskserver.task.process

import com.sos.scheduler.engine.base.process.ProcessSignal.{SIGKILL, SIGTERM}
import com.sos.scheduler.engine.common.scalautil.Closers.implicits.RichClosersCloser
import com.sos.scheduler.engine.common.scalautil.Closers.withCloser
import com.sos.scheduler.engine.common.scalautil.FileUtils.implicits.RichPath
import com.sos.scheduler.engine.common.system.OperatingSystem.isWindows
import com.sos.scheduler.engine.common.time.ScalaTime._
import com.sos.scheduler.engine.common.time.WaitForCondition.waitForCondition
import com.sos.scheduler.engine.data.job.ReturnCode
import com.sos.scheduler.engine.taskserver.task.process.StdoutStderr.Stdout
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
    val echo = "FIRSTLINE"
    val exitCode = 42
    withCloser { closer ⇒
      val stdFileMap = RichProcess.createTemporaryStdFiles()
      closer.onClose { RichProcess.tryDeleteFiles(stdFileMap.values) }
      val shellProcess = RichProcess.startShellScript(
        ProcessConfiguration(
          stdFileMap,
          additionalEnvironment = Map(envName → envValue)),
        name = "TEST",
        scriptString = s"echo $echo\nexit $exitCode")
      assert(shellProcess.files.size == 3)
      val returnCode = shellProcess.waitForTermination()
      assert(returnCode == ReturnCode(exitCode))
      assert(!shellProcess.closed.isCompleted)
      shellProcess.close()
      assert(shellProcess.closed.isCompleted)
      assert(stdFileMap(Stdout).contentString contains echo)
    }
  }

  "sendProcessSignal SIGKILL" in {
    val script = if (isWindows) "ping -n 60 127.0.0.1" else "sleep 60"
    val shellProcess = RichProcess.startShellScript(scriptString = script)
    sleep(3.s)
    assert(shellProcess.isAlive)
    shellProcess.sendProcessSignal(SIGKILL)
    waitForCondition(10.s, 100.ms) { !shellProcess.isAlive }
    assert(!shellProcess.isAlive)
    shellProcess.waitForTermination()
    shellProcess.close()
  }

  if (!isWindows) {
    "sendProcessSignal SIGTERM (Unix only)" in {
      val script = "trap 'exit 7' SIGTERM; sleep 1; sleep 1; sleep 1;sleep 1; sleep 1; sleep 1;sleep 1; sleep 1; sleep 1; exit 3"
      val shellProcess = RichProcess.startShellScript(scriptString = script)
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
