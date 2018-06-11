package com.sos.jobscheduler.common.http

import akka.actor.ActorSystem
import akka.http.scaladsl.model.{HttpResponse, Uri}
import cats.data.Validated.Valid
import com.sos.jobscheduler.common.http.AkkaHttpClientTest._
import monix.execution.Scheduler.Implicits.global
import org.scalatest.{BeforeAndAfterAll, FreeSpec}
import scala.concurrent.Await
import scala.concurrent.duration._

/**
  * @author Joacim Zschimmer
  */
final class AkkaHttpClientTest extends FreeSpec with BeforeAndAfterAll
{
  private lazy val actorSystem = ActorSystem("AkkaHttpClientTest")

  private lazy val agentClient = new AkkaHttpClient {
    protected def actorSystem = AkkaHttpClientTest.this.actorSystem
    protected val baseUri = Uri("https://example.com:9999")
    protected def uriPrefixPath = "/PREFIX"
    protected def sessionToken = None
  }

  override def afterAll() = {
    actorSystem.terminate()
    super.afterAll()
  }

  "toCheckedAgentUri, checkAgentUri and apply, failing" - {
    for ((uri, None) ← Setting) s"$uri" in {
      assert(agentClient.checkAgentUri(uri).isInvalid)
      assert(agentClient.toCheckedAgentUri(uri).isInvalid)
      assert(Await.result(agentClient.get_[HttpResponse](uri).runAsync.failed, 99.seconds).getMessage
        contains "does not match")
    }
  }

  "normalizeAgentUri" - {
    for ((uri, Some(converted)) ← Setting) s"$uri" in {
      assert(agentClient.normalizeAgentUri(uri) == converted)
      assert(agentClient.toCheckedAgentUri(uri) == Valid(converted))
    }
  }
}

object AkkaHttpClientTest {
  private val Setting = List[(Uri, Option[Uri])](
    Uri("http://example.com:9999") →
      None,
    Uri("http://example.com:9999/PREFIX/api") →
      None,
    Uri("https://example.net:9999/PREFIX/api") →
      None,
    Uri("https://example.com:7777/PREFIX/api") →
      None,
    Uri("https://example.com:9999/jobscheduler/invalid") →
      None,
    Uri("https://example.com:9999/jobscheduler/invalid/api") →
      None,
    Uri("//example.com:9999/PREFIX/api") →
      None,
    Uri("https:/PREFIX/api") →
      None,
    Uri("/jobscheduler/invalid") →
      None,
    Uri("https://example.com:9999/PREFIX") →
      Some(Uri("https://example.com:9999/PREFIX")),
    Uri("https://example.com:9999/PREFIX/api?q=1") →
      Some(Uri("https://example.com:9999/PREFIX/api?q=1")),
    Uri("/PREFIX") →
      Some(Uri("https://example.com:9999/PREFIX")),
    Uri("/PREFIX/api?q=1") →
      Some(Uri("https://example.com:9999/PREFIX/api?q=1")))
}
