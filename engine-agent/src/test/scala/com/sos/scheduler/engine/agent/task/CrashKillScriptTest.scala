package com.sos.scheduler.engine.agent.task

import com.sos.scheduler.engine.agent.data.{AgentTaskId, ProcessKillScript}
import com.sos.scheduler.engine.common.process.Processes.Pid
import com.sos.scheduler.engine.common.scalautil.AutoClosing.autoClosing
import com.sos.scheduler.engine.common.scalautil.FileUtils.autoDeleting
import com.sos.scheduler.engine.common.scalautil.FileUtils.implicits._
import com.sos.scheduler.engine.common.scalautil.HasCloser
import com.sos.scheduler.engine.common.system.OperatingSystem.isWindows
import com.sos.scheduler.engine.data.job.TaskId
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
    crashKillScript.add(AgentTaskId("1-111"), pid = None, TaskId(1), jobPath = "/folder/one")
    assert(file.contentString == """"test-kill.sh" -kill-agent-task-id=1-111 -master-task-id=1 -job=/folder/one""" + (if (isWindows) "\r\n" else "\n"))
  }

  "add more" in {
    crashKillScript.add(AgentTaskId("2-222"), pid = Some(Pid(123)), TaskId(2), jobPath = "/folder/two")
    crashKillScript.add(AgentTaskId("3-333"), pid = None, TaskId(3), jobPath = "/folder/three")
    assert(lines == List(""""test-kill.sh" -kill-agent-task-id=1-111 -master-task-id=1 -job=/folder/one""",
                         """"test-kill.sh" -kill-agent-task-id=2-222 -pid=123 -master-task-id=2 -job=/folder/two""",
                         """"test-kill.sh" -kill-agent-task-id=3-333 -master-task-id=3 -job=/folder/three"""))
  }

  "remove" in {
    crashKillScript.remove(AgentTaskId("2-222"))
    assert(lines.toSet == Set(""""test-kill.sh" -kill-agent-task-id=1-111 -master-task-id=1 -job=/folder/one""",
                              """"test-kill.sh" -kill-agent-task-id=3-333 -master-task-id=3 -job=/folder/three"""))
  }

  "add then remove" in {
    crashKillScript.add(AgentTaskId("4-444"), pid = None, TaskId(4), jobPath = "/folder/four")
    crashKillScript.remove(AgentTaskId("3-333"))
    assert(lines.toSet == Set(""""test-kill.sh" -kill-agent-task-id=1-111 -master-task-id=1 -job=/folder/one""",
                              """"test-kill.sh" -kill-agent-task-id=4-444 -master-task-id=4 -job=/folder/four"""))
  }

  "remove last" in {
    crashKillScript.remove(AgentTaskId("1-111"))
    crashKillScript.remove(AgentTaskId("4-444"))
    assert(!exists(file))
  }

  "add again and remove last" in {
    crashKillScript.add(AgentTaskId("5-5555"), pid = None, TaskId(5), jobPath = "/folder/five")
    assert(lines == List(""""test-kill.sh" -kill-agent-task-id=5-5555 -master-task-id=5 -job=/folder/five"""))
    crashKillScript.remove(AgentTaskId("5-5555"))
    assert(!exists(file))
  }

  "Tries to suppress code injection" in {
    val evilJobPaths = Vector("/x$(evil)", "/x|evil ", "/x'|evil")
    for ((evil, i) ← evilJobPaths.zipWithIndex) {
      crashKillScript.add(AgentTaskId(s"$i"), pid = None, TaskId(i), jobPath = evil)
    }
    assert(lines.toSet == (evilJobPaths.indices map { i ⇒ s""""test-kill.sh" -kill-agent-task-id=$i -master-task-id=$i""" }).toSet)
  }

  private def lines = autoClosing(io.Source.fromFile(file)) { _.getLines.toList }
}
