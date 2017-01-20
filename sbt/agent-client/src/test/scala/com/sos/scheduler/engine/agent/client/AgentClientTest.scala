package com.sos.scheduler.engine.agent.client

import akka.actor.ActorRefFactory
import com.sos.scheduler.engine.agent.client.AgentClientTest._
import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.junit.JUnitRunner
import spray.client.pipelining.Get
import spray.http.{HttpResponse, Uri}

/**
  * @author Joacim Zschimmer
  */
@RunWith(classOf[JUnitRunner])
final class AgentClientTest extends FreeSpec {

  private val agentUri = Uri("https://example.com:9999")
  private val agentClient = AgentClient(agentUri)(null: ActorRefFactory)

  "toCheckedAgentUri, checkAgentUri and apply, failing" - {
    for ((uri, None) ← Setting ) s"$uri" in {
      assert(agentClient.checkAgentUri(uri) == None)
      assert(agentClient.toCheckedAgentUri(uri) == None)
      assert(agentClient.sendReceive[HttpResponse](Get(uri)).failed.value.get.get.getMessage contains "does not match")
      assert(agentClient.sendReceiveWithHeaders[HttpResponse](Get(uri), headers = Nil).failed.value.get.get.getMessage contains "does not match")
    }
  }

  "normalizeAgentUri" - {
    for ((uri, Some(converted)) ← Setting) s"$uri" in {
      assert(agentClient.normalizeAgentUri(uri) == converted)
      assert(agentClient.toCheckedAgentUri(uri) == Some(converted))
    }
  }
}

object AgentClientTest {
  private val Setting = List[(Uri, Option[Uri])](
    Uri("http://example.com:9999") →
      None,
    Uri("http://example.com:9999/jobscheduler/agent/api") →
      None,
    Uri("https://example.net:9999/jobscheduler/agent/api") →
      None,
    Uri("https://example.com:7777/jobscheduler/agent/api") →
      None,
    Uri("https://example.com:9999/jobscheduler/invalid") →
      None,
    Uri("https://example.com:9999/jobscheduler/invalid/api") →
      None,
    Uri("//example.com:9999/jobscheduler/agent/api") →
      None,
    Uri("https:/jobscheduler/agent/api") →
      None,
    Uri("/jobscheduler/invalid") →
      None,
    Uri("https://example.com:9999/jobscheduler/agent") →
      Some(Uri("https://example.com:9999/jobscheduler/agent")),
    Uri("https://example.com:9999/jobscheduler/agent/api?q=1") →
      Some(Uri("https://example.com:9999/jobscheduler/agent/api?q=1")),
    Uri("/jobscheduler/agent") →
      Some(Uri("https://example.com:9999/jobscheduler/agent")),
    Uri("/jobscheduler/agent/api?q=1") →
      Some(Uri("https://example.com:9999/jobscheduler/agent/api?q=1")))
}
