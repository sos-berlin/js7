package js7.agent.data

import java.nio.file.Paths
import js7.common.process.Processes.Pid
import js7.data.job.TaskId
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class ProcessKillScriptTest extends AnyFreeSpec {

  "toCommandArguments" in {
    val killScript = ProcessKillScript(Paths.get("KILL-SCRIPT"))
    assert(killScript.toCommandArguments(AgentTaskId(1, 2), None, TaskId(123)) ==
      List("KILL-SCRIPT", "-kill-agent-task-id=1-2", "-master-task-id=123"))
    assert(killScript.toCommandArguments(AgentTaskId(1, 2), Some(Pid(777)), TaskId(123)) ==
      List("KILL-SCRIPT", "-kill-agent-task-id=1-2", "-pid=777", "-master-task-id=123"))
  }
}
