package com.sos.scheduler.engine.taskserver.task.process

import com.google.common.io.Files.touch
import com.sos.scheduler.engine.agent.data.AgentTaskId
import com.sos.scheduler.engine.common.scalautil.AutoClosing.autoClosing
import com.sos.scheduler.engine.common.scalautil.FileUtils.implicits._
import com.sos.scheduler.engine.common.scalautil.SideEffect.ImplicitSideEffect
import com.sos.scheduler.engine.common.system.FileUtils._
import com.sos.scheduler.engine.common.system.OperatingSystem.{isUnix, isWindows}
import com.sos.scheduler.engine.common.time.ScalaTime._
import java.nio.file.Files
import java.nio.file.Files.{createTempFile, delete, deleteIfExists, exists, size}
import java.nio.file.attribute.PosixFileAttributes
import java.nio.file.attribute.PosixFilePermission.OWNER_EXECUTE
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
    autoClosing(new ProcessKillScriptProvider) { provider ⇒
      val killScript = provider.provideTo(temporaryDirectory)
      val out = createTempFile("test-", ".log")
      val script = if (isWindows)
        createTempFile("test-", ".cmd") sideEffect { _.contentString = "ping -n 100 127.0.0.1" }
      else
        createTempFile("test-", ".sh") sideEffect { file ⇒
          delete(file)
          Files.createFile(file, Processes.shellFileAttributes: _*)
          file.contentString = "ping -c 100 127.0.0.1"
        }
      val agentTaskId = AgentTaskId("1-TEST")
      val processBuilder = new ProcessBuilder(script.toString, s"-agent-task-id=${agentTaskId.string}")
      processBuilder.redirectOutput(out)
      val process = processBuilder.start()
      sleep(1.s)
      val killTerminated = sys.runtime.exec(killScript.toCommandArguments(agentTaskId).toArray).waitFor(60, SECONDS)
      assert(killTerminated)
      val terminated = process.waitFor(60, SECONDS)
      assert(terminated)
      sleep(1.s)  // Time to let kill take effect
      val outSize = size(out)
      sleep(2.s)
      assert(outSize == size(out), "Stdout file must not grow after kill script")
      delete(script)
      delete(out)
    }
  }
}
