package com.sos.jobscheduler.agent.data.web

import com.sos.jobscheduler.agent.data.AgentTaskId
import org.scalatest.FreeSpec

/**
 * @author Joacim Zschimmer
 */
final class AgentUrisTest extends FreeSpec {

  private val agentUris = AgentUris("http://example.com:9999/testPrefix")

  "command" in {
    assert(agentUris.command ==
      "http://example.com:9999/testPrefix/jobscheduler/agent/api/command")
  }

  "fileExists" in {
    assert(agentUris.fileExists("/FILE X+") ==
      "http://example.com:9999/testPrefix/jobscheduler/agent/api/fileExists?file=/FILE+X%2B")
  }

  "task" - {
    "overview" in {
      assert(agentUris.task.overview ==
        "http://example.com:9999/testPrefix/jobscheduler/agent/api/task")
    }

    "tasks" in {
      assert(agentUris.task.tasks ==
        "http://example.com:9999/testPrefix/jobscheduler/agent/api/task/")
    }

    "task" in {
      assert(agentUris.task.tasks ==
        "http://example.com:9999/testPrefix/jobscheduler/agent/api/task/")
    }
  }

  "task.tasks" in {
    assert(agentUris.task(AgentTaskId("TASK-ID")) ==
      "http://example.com:9999/testPrefix/jobscheduler/agent/api/task/TASK-ID")
  }

  "overview" in {
    assert(agentUris.overview ==
      "http://example.com:9999/testPrefix/jobscheduler/agent/api")
  }
}
