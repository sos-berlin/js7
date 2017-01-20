package com.sos.scheduler.engine.agent.data.web

import com.sos.scheduler.engine.agent.data.AgentTaskId
import com.sos.scheduler.engine.tunnel.data.TunnelId
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

  "tunnel" - {
    "overview" in {
      assert(agentUris.tunnel.overview ==
        "http://example.com:9999/testPrefix/jobscheduler/agent/api/tunnel")
    }

    "tunnels" in {
      assert(agentUris.tunnel.tunnels ==
      "http://example.com:9999/testPrefix/jobscheduler/agent/api/tunnel/")
    }

    "tunnel" in {
      assert(agentUris.tunnel(TunnelId("TUNNEL-ID")) ==
        "http://example.com:9999/testPrefix/jobscheduler/agent/api/tunnel/TUNNEL-ID")
    }
  }

  "overview" in {
    assert(agentUris.overview ==
      "http://example.com:9999/testPrefix/jobscheduler/agent/api")
  }
}
