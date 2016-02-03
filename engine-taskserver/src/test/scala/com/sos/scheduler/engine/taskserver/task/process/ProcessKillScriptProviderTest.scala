package com.sos.scheduler.engine.taskserver.task.process

import com.google.common.io.Files.touch
import com.sos.scheduler.engine.agent.data.AgentTaskId
import com.sos.scheduler.engine.base.process.ProcessSignal.SIGKILL
import com.sos.scheduler.engine.common.scalautil.AutoClosing.autoClosing
import com.sos.scheduler.engine.common.scalautil.FileUtils.implicits._
import com.sos.scheduler.engine.common.scalautil.SideEffect.ImplicitSideEffect
import com.sos.scheduler.engine.common.system.FileUtils._
import com.sos.scheduler.engine.common.system.OperatingSystem.{isUnix, isWindows}
import com.sos.scheduler.engine.common.time.ScalaTime._
import java.nio.file.Files._
import java.nio.file.attribute.PosixFileAttributes
import java.nio.file.attribute.PosixFilePermission.OWNER_EXECUTE
import java.nio.file.{Files, Path}
import java.util.concurrent.TimeUnit.SECONDS
import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.junit.JUnitRunner

/**
  * JS-1558 Agent includes kill scripts
  *
  * @author Joacim Zschimmer
  */
@RunWith(classOf[JUnitRunner])
final class ProcessKillScriptProviderTest extends FreeSpec {

  private lazy val tmp = temporaryDirectory
  val expectedFile = tmp / (if (isWindows) "jobscheduler_agent_kill_task.cmd" else "jobscheduler_agent_kill_task.sh")

  "Do nothing" in {
    val provider = new ProcessKillScriptProvider
    provider.close()
  }

  "Provide script and delete it later" in {
    val provider = new ProcessKillScriptProvider
    deleteIfExists(expectedFile)
    val killScript = provider.provideTo(tmp)
    assert(killScript.file == expectedFile)
    assert(size(expectedFile) > 0)
    if (isUnix) {
      assert(Files.readAttributes(expectedFile, classOf[PosixFileAttributes]).permissions contains OWNER_EXECUTE)
    }
    provider.close()
    assert(!exists(expectedFile))
  }

  "Existing file is overwritten" in {
    touch(expectedFile)
    val provider = new ProcessKillScriptProvider
    provider.provideTo(tmp)
    assert(exists(expectedFile))
    provider.close()
    assert(!exists(expectedFile))
  }

  "Kill script kills descendants" in {
    val out = createTempFile("test-", ".log")
    val agentTaskId = AgentTaskId("1-TEST")
    val (scriptFile, process) = startNestedProcess(agentTaskId, out)
    sleep(1.s)
    runKillScript(agentTaskId)
    process.waitFor(10, SECONDS)
    assert(process.exitValue == (if (isWindows) 1 else 128 + SIGKILL.value))
    sleep(1.s) // Time to let kill take effect
    val beforeKill = out.contentString
    sleep(2.s)
    val grown = out.contentString stripPrefix beforeKill
    assert(grown == "", "Stdout file must not grow after kill script")
    delete(scriptFile)
    delete(out)
  }

  private def startNestedProcess(agentTaskId: AgentTaskId, out: Path) = {
    val file = if (isWindows)
      createTempFile("test-", ".cmd") sideEffect { _.contentString = "ping -n 100 127.0.0.1\r\n" }
    else
      createTempFile("test-", ".sh") sideEffect { file ⇒
        delete(file)
        createFile(file, Processes.shellFileAttributes: _*)
        file.contentString = "sleep 99\n"
      }
    val b = new ProcessBuilder(file.toString, s"-agent-task-id=${agentTaskId.string}")
    b.redirectOutput(out)
    (file, b.start())
  }

  private def runKillScript(agentTaskId: AgentTaskId): Unit = {
    autoClosing(new ProcessKillScriptProvider) { provider ⇒
      val killScript = provider.provideTo(temporaryDirectory)
      val killProcess = sys.runtime.exec(killScript.toCommandArguments(agentTaskId).toArray)
      killProcess.waitFor(60, SECONDS)
      assert(killProcess.exitValue == 0)
    }
  }
}
