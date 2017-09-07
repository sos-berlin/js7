package com.sos.jobscheduler.agent.data.web

import akka.http.scaladsl.model.Uri
import com.sos.jobscheduler.agent.data.AgentTaskId
import org.scalatest.FreeSpec

/**
 * @author Joacim Zschimmer
 */
final class AgentUrisTest extends FreeSpec {

  private val agentUris = AgentUris("http://example.com:9999/testPrefix")

  "command" in {
    assert(agentUris.command ==
      Uri("http://example.com:9999/testPrefix/jobscheduler/agent/api/command"))
  }

  "task" - {
    "overview" in {
      assert(agentUris.task.overview ==
        Uri("http://example.com:9999/testPrefix/jobscheduler/agent/api/task"))
    }

    "tasks" in {
      assert(agentUris.task.tasks ==
        Uri("http://example.com:9999/testPrefix/jobscheduler/agent/api/task/"))
    }

    "task" in {
      assert(agentUris.task.tasks ==
        Uri("http://example.com:9999/testPrefix/jobscheduler/agent/api/task/"))
    }
  }

  "task.tasks" in {
    assert(agentUris.task(AgentTaskId("TASK-ID")) ==
      Uri("http://example.com:9999/testPrefix/jobscheduler/agent/api/task/TASK-ID"))
  }

  "overview" in {
    assert(agentUris.overview ==
      Uri("http://example.com:9999/testPrefix/jobscheduler/agent/api"))
  }
}
