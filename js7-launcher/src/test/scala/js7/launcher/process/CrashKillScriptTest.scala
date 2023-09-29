package js7.launcher.process

import java.nio.file.Files.{createTempFile, delete, exists, size}
import java.nio.file.Paths
import js7.base.io.file.FileUtils.implicits.*
import js7.base.io.file.FileUtils.syntax.*
import js7.base.io.process.Pid
import js7.base.system.OperatingSystem.isWindows
import js7.base.test.OurTestSuite
import js7.base.utils.AutoClosing.autoClosing
import js7.base.utils.Closer.syntax.RichClosersAutoCloseable
import js7.base.utils.HasCloser
import js7.data.job.TaskId
import js7.launcher.configuration.ProcessKillScript
import org.scalatest.BeforeAndAfterAll

/**
  * JS-1551.
  *
  * @author Joacim Zschimmer
  */
final class CrashKillScriptTest extends OurTestSuite with HasCloser with BeforeAndAfterAll
{
  override protected def afterAll() = {
    closer.close()
    super.afterAll()
  }

  private val killScript = ProcessKillScript(Paths.get("test-kill.sh"))

  "Overwrites file" in {
    val file = createTempFile("CrashKillScriptTest-", ".tmp")
    file := "garbage"
    autoClosing(new CrashKillScript(killScript = killScript, file = file)) { _ =>
      assert(size(file) == 0)
    }
    assert(!exists(file))
  }

  "Creates file" in {
    val file = createTempFile("CrashKillScriptTest-", ".tmp")
    delete(file)
    autoClosing(new CrashKillScript(killScript = killScript, file = file)) { _ =>
      assert(size(file) == 0)
    }
    assert(!exists(file))
  }

  private lazy val file = createTempFile("CrashKillScriptTest-", ".tmp")
  private lazy val crashKillScript = new CrashKillScript(killScript = killScript, file = file).closeWithCloser

  "Script is initially empty" in {
    assert(lines == Nil)
  }

  "add" in {
    crashKillScript.add(TaskId("1-111"), pid = None)
    assert(file.contentString == """"test-kill.sh" --kill-agent-task-id=1-111""" + (if isWindows then "\r\n" else "\n"))
  }

  "add more" in {
    crashKillScript.add(TaskId("2-222"), pid = Some(Pid(123)))
    crashKillScript.add(TaskId("3-333"), pid = None)
    assert(lines == List(""""test-kill.sh" --kill-agent-task-id=1-111""",
                         """"test-kill.sh" --kill-agent-task-id=2-222 --pid=123""",
                         """"test-kill.sh" --kill-agent-task-id=3-333"""))
  }

  "remove" in {
    crashKillScript.remove(TaskId("2-222"))
    assert(lines.toSet == Set(""""test-kill.sh" --kill-agent-task-id=1-111""",
                              """"test-kill.sh" --kill-agent-task-id=3-333"""))
  }

  "add then remove" in {
    crashKillScript.add(TaskId("4-444"), pid = None)
    assert(lines.toSet == Set(""""test-kill.sh" --kill-agent-task-id=1-111""",
                              """"test-kill.sh" --kill-agent-task-id=3-333""",
                              """"test-kill.sh" --kill-agent-task-id=4-444"""))
  }

  "remove again" in {
    crashKillScript.remove(TaskId("3-333"))
    assert(lines.toSet == Set(""""test-kill.sh" --kill-agent-task-id=1-111""",
                              """"test-kill.sh" --kill-agent-task-id=4-444"""))
  }

  "remove last" in {
    crashKillScript.remove(TaskId("1-111"))
    crashKillScript.remove(TaskId("4-444"))
    assert(!exists(file))
  }

  "add again and remove last" in {
    crashKillScript.add(TaskId("5-5555"), pid = None)
    assert(lines == List(""""test-kill.sh" --kill-agent-task-id=5-5555"""))
    crashKillScript.remove(TaskId("5-5555"))
    assert(!exists(file))
  }

  "Tries to suppress code injection" in {
    val evilJobPaths = Vector("/x$(evil)", "/x|evil ", "/x'|evil")
    for (evilJobPath, i) <- evilJobPaths.zipWithIndex do {
      crashKillScript.add(TaskId(s"$i"), pid = None)
    }
    assert(lines.toSet == (evilJobPaths.indices map { i => s""""test-kill.sh" --kill-agent-task-id=$i""" }).toSet)
    for i <- evilJobPaths.indices do {
      crashKillScript.remove(TaskId(s"$i"))
    }
  }

  "close with left tasks does not delete file" in {
    crashKillScript.add(TaskId("LEFT"), pid = None)
    crashKillScript.close()
    assert(lines == s""""test-kill.sh" --kill-agent-task-id=LEFT""" :: Nil)
  }

  private def lines = autoClosing(scala.io.Source.fromFile(file))(_.getLines().toList)
}
