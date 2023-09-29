package js7.agent.data.web

import js7.base.test.OurTestSuite
import js7.base.web.Uri

/**
 * @author Joacim Zschimmer
 */
final class AgentUrisTest extends OurTestSuite:
  private val agentUris = AgentUris(Uri("https://example.com:9999/testPrefix"))

  "command" in:
    assert(agentUris.command ==
      Uri("https://example.com:9999/testPrefix/agent/api/command"))

  "session" in:
    assert(agentUris.session ==
      Uri("https://example.com:9999/testPrefix/agent/api/session"))

  "overview" in:
    assert(agentUris.overview ==
      Uri("https://example.com:9999/testPrefix/agent/api"))

  "Trailing slash in URI is ignored" in:
    assert(AgentUris(Uri("https://example.com:9999")).overview == Uri("https://example.com:9999/agent/api"))
    assert(AgentUris(Uri("https://example.com:9999/")).overview == Uri("https://example.com:9999/agent/api"))
    assert(AgentUris(Uri("https://example.com:9999/x")).overview == Uri("https://example.com:9999/x/agent/api"))
    assert(AgentUris(Uri("https://example.com:9999/x/")).overview == Uri("https://example.com:9999/x/agent/api"))
