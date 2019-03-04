package com.sos.jobscheduler.agent.task

import com.sos.jobscheduler.agent.data.{AgentTaskId, ProcessKillScript}
import com.sos.jobscheduler.common.process.Processes.Pid
import com.sos.jobscheduler.common.scalautil.AutoClosing.autoClosing
import com.sos.jobscheduler.common.scalautil.Closer.ops.RichClosersAutoCloseable
import com.sos.jobscheduler.common.scalautil.FileUtils.implicits._
import com.sos.jobscheduler.common.scalautil.HasCloser
import com.sos.jobscheduler.common.system.OperatingSystem.isWindows
import com.sos.jobscheduler.data.job.TaskId
import java.nio.file.Files.{delete, exists, size}
import java.nio.file.{Files, Paths}
import org.scalatest.{BeforeAndAfterAll, FreeSpec}

/**
  * JS-1551.
  *
  * @author Joacim Zschimmer
  */
final class CrashKillScriptTest extends FreeSpec with HasCloser with BeforeAndAfterAll {

  override protected def afterAll() = closer.close()

  private val killScript = ProcessKillScript(Paths.get("test-kill.sh"))

  "Overwrites file" in {
    val file = Files.createTempFile("CrashKillScriptTest-", ".tmp")
    file := "garbage"
    autoClosing(new CrashKillScript(killScript = killScript, file = file)) { _ =>
      assert(size(file) == 0)
    }
    assert(!exists(file))
  }

  "Creates file" in {
    val file = Files.createTempFile("CrashKillScriptTest-", ".tmp")
    delete(file)
    autoClosing(new CrashKillScript(killScript = killScript, file = file)) { _ =>
      assert(size(file) == 0)
    }
    assert(!exists(file))
  }

  private lazy val file = Files.createTempFile("CrashKillScriptTest-", ".tmp")
  private lazy val crashKillScript = new CrashKillScript(killScript = killScript, file = file).closeWithCloser

  "Script is initially empty" in {
    assert(lines == Nil)
  }

  "add" in {
    crashKillScript.add(AgentTaskId("1-111"), pid = None, TaskId(1))
    assert(file.contentString == """"test-kill.sh" -kill-agent-task-id=1-111 -master-task-id=1""" + (if (isWindows) "\r\n" else "\n"))
  }

  "add more" in {
    crashKillScript.add(AgentTaskId("2-222"), pid = Some(Pid(123)), TaskId(2))
    crashKillScript.add(AgentTaskId("3-333"), pid = None, TaskId(3))
    assert(lines == List(""""test-kill.sh" -kill-agent-task-id=1-111 -master-task-id=1""",
                         """"test-kill.sh" -kill-agent-task-id=2-222 -pid=123 -master-task-id=2""",
                         """"test-kill.sh" -kill-agent-task-id=3-333 -master-task-id=3"""))
  }

  "remove" in {
    crashKillScript.remove(AgentTaskId("2-222"))
    assert(lines.toSet == Set(""""test-kill.sh" -kill-agent-task-id=1-111 -master-task-id=1""",
                              """"test-kill.sh" -kill-agent-task-id=3-333 -master-task-id=3"""))
  }

  "add then remove" in {
    crashKillScript.add(AgentTaskId("4-444"), pid = None, TaskId(4))
    assert(lines.toSet == Set(""""test-kill.sh" -kill-agent-task-id=1-111 -master-task-id=1""",
                              """"test-kill.sh" -kill-agent-task-id=3-333 -master-task-id=3""",
                              """"test-kill.sh" -kill-agent-task-id=4-444 -master-task-id=4"""))
  }

  "remove again" in {
    crashKillScript.remove(AgentTaskId("3-333"))
    assert(lines.toSet == Set(""""test-kill.sh" -kill-agent-task-id=1-111 -master-task-id=1""",
                              """"test-kill.sh" -kill-agent-task-id=4-444 -master-task-id=4"""))
  }

  "remove last" in {
    crashKillScript.remove(AgentTaskId("1-111"))
    crashKillScript.remove(AgentTaskId("4-444"))
    assert(!exists(file))
  }

  "add again and remove last" in {
    crashKillScript.add(AgentTaskId("5-5555"), pid = None, TaskId(5))
    assert(lines == List(""""test-kill.sh" -kill-agent-task-id=5-5555 -master-task-id=5"""))
    crashKillScript.remove(AgentTaskId("5-5555"))
    assert(!exists(file))
  }

  "Tries to suppress code injection" in {
    val evilJobPaths = Vector("/x$(evil)", "/x|evil ", "/x'|evil")
    for ((evilJobPath, i) <- evilJobPaths.zipWithIndex) {
      crashKillScript.add(AgentTaskId(s"$i"), pid = None, TaskId(i))
    }
    assert(lines.toSet == (evilJobPaths.indices map { i => s""""test-kill.sh" -kill-agent-task-id=$i -master-task-id=$i""" }).toSet)
    for (i <- evilJobPaths.indices) {
      crashKillScript.remove(AgentTaskId(s"$i"))
    }
  }

  "close with left tasks does not delete file" in {
    crashKillScript.add(AgentTaskId("LEFT"), pid = None, TaskId(999))
    crashKillScript.close()
    assert(lines == s""""test-kill.sh" -kill-agent-task-id=LEFT -master-task-id=999""" :: Nil)
  }

  private def lines = autoClosing(scala.io.Source.fromFile(file)) { _.getLines.toList }
}
