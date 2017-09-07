package com.sos.jobscheduler.agent.client

import akka.actor.ActorSystem
import akka.http.scaladsl.model.{HttpResponse, Uri}
import com.sos.jobscheduler.agent.client.AgentClientTest._
import com.sos.jobscheduler.common.scalautil.Futures.implicits._
import com.sos.jobscheduler.common.time.ScalaTime._
import org.scalatest.{BeforeAndAfterAll, FreeSpec}

/**
  * @author Joacim Zschimmer
  */
final class AgentClientTest extends FreeSpec with BeforeAndAfterAll {

  private val agentUri = Uri("https://example.com:9999")
  private lazy val actorSystem = ActorSystem("AgentClientTest")
  private lazy val agentClient = AgentClient(agentUri)(actorSystem)

  override def afterAll() = {
    actorSystem.terminate()
    super.afterAll()
  }

  "toCheckedAgentUri, checkAgentUri and apply, failing" - {
    for ((uri, None) ← Setting) s"$uri" in {
      assert(agentClient.checkAgentUri(uri) == None)
      assert(agentClient.toCheckedAgentUri(uri) == None)
      assert((agentClient.getUri[HttpResponse](uri).failed await 99.s).getMessage contains "does not match")
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
