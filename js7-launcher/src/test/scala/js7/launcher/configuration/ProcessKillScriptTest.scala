package js7.launcher.configuration

import java.nio.file.Paths
import js7.base.io.process.Pid
import js7.base.test.OurTestSuite
import js7.data.job.TaskId

/**
  * @author Joacim Zschimmer
  */
final class ProcessKillScriptTest extends OurTestSuite:

  "toCommandArguments" in:
    assert:
      ProcessKillScript(Paths.get("KILL-SCRIPT")).toCommandArguments(TaskId(1, 2), Pid(777)) ==
        List("KILL-SCRIPT", "--kill-agent-task-id=1-2", "--pid=777")
