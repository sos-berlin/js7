package com.sos.scheduler.engine.agent.task

import com.sos.scheduler.engine.agent.data.{AgentTaskId, ProcessKillScript}
import com.sos.scheduler.engine.common.scalautil.AutoClosing.autoClosing
import com.sos.scheduler.engine.common.scalautil.FileUtils.autoDeleting
import com.sos.scheduler.engine.common.scalautil.FileUtils.implicits._
import com.sos.scheduler.engine.common.scalautil.HasCloser
import com.sos.scheduler.engine.common.system.OperatingSystem.isWindows
import java.nio.file.Files.{delete, exists, size}
import java.nio.file.{Files, Paths}
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{BeforeAndAfterAll, FreeSpec}

/**
  * JS-1551.
  *
  * @author Joacim Zschimmer
  */
@RunWith(classOf[JUnitRunner])
final class CrashKillScriptTest extends FreeSpec with HasCloser with BeforeAndAfterAll {

  override protected def afterAll() = closer.close()

  private val killScript = ProcessKillScript(Paths.get("test-kill.sh"))

  "Overwrites file" in {
    autoDeleting(Files.createTempFile("CrashKillScriptTest-", ".tmp")) { file ⇒
      file.contentString = "garbage"
      new CrashKillScript(killScript = killScript, file = file)
      assert(size(file) == 0)
    }
  }

  "Creates file" in {
    autoDeleting(Files.createTempFile("CrashKillScriptTest-", ".tmp")) { file ⇒
      delete(file)
      new CrashKillScript(killScript = killScript, file = file)
      assert(size(file) == 0)
    }
  }

  private lazy val file = Files.createTempFile("CrashKillScriptTest-", ".tmp")
  private lazy val crashKillScript = new CrashKillScript(killScript = killScript, file = file)

  "Script is initially empty" in {
    assert(lines == Nil)
  }

  "add" in {
    crashKillScript.add(AgentTaskId("1-111"))
    assert(file.contentString == "test-kill.sh -kill-agent-task-id=1-111" + (if (isWindows) "\r\n" else "\n"))
  }

  "add more" in {
    crashKillScript.add(AgentTaskId("2-222"))
    crashKillScript.add(AgentTaskId("3-333"))
    assert(lines == List("test-kill.sh -kill-agent-task-id=1-111",
                         "test-kill.sh -kill-agent-task-id=2-222",
                         "test-kill.sh -kill-agent-task-id=3-333"))
  }

  "remove" in {
    crashKillScript.remove(AgentTaskId("2-222"))
    assert(lines.toSet == Set("test-kill.sh -kill-agent-task-id=1-111",
                              "test-kill.sh -kill-agent-task-id=3-333"))
  }

  "add then remove" in {
    crashKillScript.add(AgentTaskId("4-444"))
    crashKillScript.remove(AgentTaskId("3-333"))
    assert(lines.toSet == Set("test-kill.sh -kill-agent-task-id=1-111",
                              "test-kill.sh -kill-agent-task-id=4-444"))
  }

  "remove last" in {
    crashKillScript.remove(AgentTaskId("1-111"))
    crashKillScript.remove(AgentTaskId("4-444"))
    assert(!exists(file))
  }

  "add again and remove last" in {
    crashKillScript.add(AgentTaskId("5-5555"))
    assert(lines == List("test-kill.sh -kill-agent-task-id=5-5555"))
    crashKillScript.remove(AgentTaskId("5-5555"))
    assert(!exists(file))
  }

  private def lines = autoClosing(io.Source.fromFile(file)) { _.getLines.toList }
}
