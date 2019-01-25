package com.sos.jobscheduler.common.http

import akka.actor.ActorSystem
import akka.http.scaladsl.model.ContentTypes.{`application/json`, `text/plain(UTF-8)`}
import akka.http.scaladsl.model.StatusCodes.BadRequest
import akka.http.scaladsl.model.headers.`Content-Type`
import akka.http.scaladsl.model.{HttpEntity, HttpResponse, Uri}
import cats.data.Validated.{Invalid, Valid}
import com.sos.jobscheduler.base.circeutils.CirceUtils._
import com.sos.jobscheduler.base.problem.Problem
import com.sos.jobscheduler.common.http.AkkaHttpClient.{HttpException, liftProblem}
import com.sos.jobscheduler.common.http.AkkaHttpClientTest._
import java.nio.charset.StandardCharsets.UTF_8
import monix.eval.Task
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
      assert(Await.result(agentClient.get_[HttpResponse](uri).runToFuture.failed, 99.seconds).getMessage
        contains "does not match")
    }
  }

  "normalizeAgentUri" - {
    for ((uri, Some(converted)) ← Setting) s"$uri" in {
      assert(agentClient.normalizeAgentUri(uri) == converted)
      assert(agentClient.toCheckedAgentUri(uri) == Valid(converted))
    }
  }

  "liftProblem, HttpException#problem" - {
    "No Exception" in {
      assert(liftProblem(Task(1)).runSyncUnsafe(99.seconds) == Valid(1))
    }

    "HttpException with problem" in {
      val problem = Problem.pure("PROBLEM")
      val jsonString = Problem.typedJsonEncoder.encodeObject(problem).compactPrint
      val e = new HttpException(
        HttpResponse(BadRequest, entity = HttpEntity(`application/json`, jsonString.getBytes(UTF_8))),
        Uri("/URI"),
        jsonString)
      assert(e.problem == Some(problem))
      assert(liftProblem(Task.raiseError(e)).runSyncUnsafe(99.seconds) == Invalid(problem))
    }

    "HttpException with broken problem" in {
      val jsonString = "{}"
      val e = new HttpException(
        HttpResponse(BadRequest, entity = HttpEntity(`application/json`, jsonString.getBytes(UTF_8))),
        Uri("/URI"),
        jsonString)
      assert(e.problem.isEmpty)
      assert(liftProblem(Task.raiseError(e)).failed.runSyncUnsafe(99.seconds) eq e)
    }

    "HttpException with string response" in {
      val e = new HttpException(
        HttpResponse(BadRequest, `Content-Type`(`text/plain(UTF-8)`) :: Nil),
        Uri("/URI"),
        "{}")
      assert(liftProblem(Task.raiseError(e)).failed.runSyncUnsafe(99.seconds) eq e)
    }

    "Other exception" in {
      val e = new Exception
      assert(liftProblem(Task.raiseError(e)).failed.runSyncUnsafe(99.seconds) eq e)
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
