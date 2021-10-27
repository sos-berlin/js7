package js7.launcher.configuration

import java.nio.file.Paths
import js7.base.io.process.Processes.Pid
import js7.data.job.TaskId
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class ProcessKillScriptTest extends AnyFreeSpec {

  "toCommandArguments" in {
    val killScript = ProcessKillScript(Paths.get("KILL-SCRIPT"))
    assert(killScript.toCommandArguments(TaskId(1, 2), None) ==
      List("KILL-SCRIPT", "--kill-agent-task-id=1-2"))
    assert(killScript.toCommandArguments(TaskId(1, 2), Some(Pid(777))) ==
      List("KILL-SCRIPT", "--kill-agent-task-id=1-2", "--pid=777"))
  }
}
