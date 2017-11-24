package com.sos.jobscheduler.agent.data

import com.sos.jobscheduler.common.process.Processes.Pid
import com.sos.jobscheduler.data.job.TaskId
import com.sos.jobscheduler.data.workflow.JobPath
import java.nio.file.Paths
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class ProcessKillScriptTest extends FreeSpec {

  "toCommandArguments" in {
    val killScript = ProcessKillScript(Paths.get("KILL-SCRIPT"))
    assert(killScript.toCommandArguments(AgentTaskId(1, 2), None, JobPath("/JOB"), TaskId(123)) ==
      List("KILL-SCRIPT", "-kill-agent-task-id=1-2", "-master-task-id=123", "-job=/JOB"))
    assert(killScript.toCommandArguments(AgentTaskId(1, 2), Some(Pid(777)), JobPath("/JOB"), TaskId(123)) ==
      List("KILL-SCRIPT", "-kill-agent-task-id=1-2", "-pid=777", "-master-task-id=123", "-job=/JOB"))
  }
}
