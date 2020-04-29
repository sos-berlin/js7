package com.sos.jobscheduler.common.http

import akka.actor.ActorSystem
import akka.http.scaladsl.model.ContentTypes.{`application/json`, `text/plain(UTF-8)`}
import akka.http.scaladsl.model.StatusCodes.BadRequest
import akka.http.scaladsl.model.headers.`Content-Type`
import akka.http.scaladsl.model.{HttpEntity, HttpResponse}
import com.sos.jobscheduler.base.circeutils.CirceUtils._
import com.sos.jobscheduler.base.problem.Problem
import com.sos.jobscheduler.base.time.ScalaTime._
import com.sos.jobscheduler.base.web.HttpClient.liftProblem
import com.sos.jobscheduler.base.web.Uri
import com.sos.jobscheduler.common.http.AkkaHttpClient.HttpException
import com.sos.jobscheduler.common.http.AkkaHttpClientTest._
import java.net.ServerSocket
import java.nio.charset.StandardCharsets.UTF_8
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import org.scalatest.BeforeAndAfterAll
import org.scalatest.freespec.AnyFreeSpec
import scala.concurrent.duration.Deadline.now
import scala.concurrent.duration._
import scala.concurrent.{Await, TimeoutException}

/**
  * @author Joacim Zschimmer
  */
final class AkkaHttpClientTest extends AnyFreeSpec with BeforeAndAfterAll
{
  private lazy val actorSystem = ActorSystem("AkkaHttpClientTest")

  private lazy val httpClient = new AkkaHttpClient {
    protected val actorSystem = AkkaHttpClientTest.this.actorSystem
    protected val baseUri = Uri("https://example.com:9999")
    protected val name = "AkkaHttpClientTest"
    protected def uriPrefixPath = "/PREFIX"
    protected def sessionToken = None
  }

  override def afterAll() = {
    actorSystem.terminate()
    super.afterAll()
  }

  "toCheckedAgentUri, checkAgentUri and apply, failing" - {
    for ((uri, None) <- Setting) s"$uri" in {
      assert(httpClient.checkAgentUri(uri).isLeft)
      assert(httpClient.toCheckedAgentUri(uri).isLeft)
      assert(Await.result(httpClient.get_[HttpResponse](uri).runToFuture.failed, 99.seconds).getMessage
        contains "does not match")
    }
  }

  "normalizeAgentUri" - {
    for ((uri, Some(converted)) <- Setting) s"$uri" in {
      assert(httpClient.normalizeAgentUri(uri) == converted)
      assert(httpClient.toCheckedAgentUri(uri) == Right(converted))
    }
  }

  "liftProblem, HttpException#problem" - {
    "No Exception" in {
      assert(liftProblem(Task(1)).runSyncUnsafe(99.seconds) == Right(1))
    }

    "HttpException with problem" in {
      val problem = Problem.pure("PROBLEM")
      val jsonString = Problem.typedJsonEncoder.encodeObject(problem).compactPrint
      val e = new HttpException(
        HttpResponse(BadRequest, entity = HttpEntity(`application/json`, jsonString.getBytes(UTF_8))),
        Uri("/URI"),
        jsonString)
      assert(e.problem == Some(problem))
      assert(liftProblem(Task.raiseError(e)).runSyncUnsafe(99.seconds) == Left(problem))
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

  "Operations after close are rejected" in {
    httpClient.close()
    val uri = Uri("https://example.com:9999/PREFIX")
    assert(Await.result(httpClient.get_[HttpResponse](uri).runToFuture.failed, 99.seconds).getMessage
      contains "»AkkaHttpClientTest« has been closed")
  }

  "Unreachable port, try several times" - {
    lazy val port = {
      val socket = new ServerSocket(0)
      val port = socket.getLocalPort
      socket.close()
      port
    }
    lazy val uri = Uri(s"http://127.0.0.1:$port")

    def newHttpClient(timeout: FiniteDuration) = new AkkaHttpClient {
      protected val actorSystem = AkkaHttpClientTest.this.actorSystem
      protected val baseUri = uri
      protected val name = "AkkaHttpClientTest"
      protected def uriPrefixPath = "/PREFIX"
      protected def sessionToken = None
    }
    var duration: FiniteDuration = null

    "First call" in {
      val httpClient = newHttpClient(99.s)
      try {
        val since = now
        val a = Await.ready(httpClient.get_(Uri(s"$uri/PREFIX/TEST")).runToFuture, 99.s - 1.s)
        duration = since.elapsed
        assert(a.value.get.isFailure)
      } finally httpClient.close()
    }

    "Second call lets akka-http block for a very long time" in {
      // Akka 2.5.30 blocks on next call after connection failure (why?),
      // so our responseTimeout kicks in.
      // TODO Replace by http4s ?
      val httpClient = newHttpClient(duration - 1.s max 5.s)
      intercept[TimeoutException] {
        try {
          val a = Await.ready(httpClient.get_(Uri(s"$uri/PREFIX/TEST")).runToFuture, duration + 1.s)
          assert(a.value.get.isFailure)
        } finally httpClient.close()
      }
    }
  }
}

object AkkaHttpClientTest
{
  private val Setting = List[(Uri, Option[Uri])](
    Uri("http://example.com:9999") ->
      None,
    Uri("http://example.com:9999/PREFIX/api") ->
      None,
    Uri("https://example.net:9999/PREFIX/api") ->
      None,
    Uri("https://example.com:7777/PREFIX/api") ->
      None,
    Uri("https://example.com:9999/jobscheduler/invalid") ->
      None,
    Uri("https://example.com:9999/jobscheduler/invalid/api") ->
      None,
    Uri("//example.com:9999/PREFIX/api") ->
      None,
    Uri("https:/PREFIX/api") ->
      None,
    Uri("/jobscheduler/invalid") ->
      None,
    Uri("https://example.com:9999/PREFIX") ->
      Some(Uri("https://example.com:9999/PREFIX")),
    Uri("https://example.com:9999/PREFIX/api?q=1") ->
      Some(Uri("https://example.com:9999/PREFIX/api?q=1")),
    Uri("/PREFIX") ->
      Some(Uri("https://example.com:9999/PREFIX")),
    Uri("/PREFIX/api?q=1") ->
      Some(Uri("https://example.com:9999/PREFIX/api?q=1")))
}
