package js7.common.http

import akka.http.scaladsl.model.ContentTypes.{`application/json`, `text/plain(UTF-8)`}
import akka.http.scaladsl.model.HttpMethods.POST
import akka.http.scaladsl.model.StatusCodes.{BadRequest, InternalServerError, OK}
import akka.http.scaladsl.model.headers.`Content-Type`
import akka.http.scaladsl.model.{HttpEntity, HttpResponse}
import akka.http.scaladsl.server.Directives._
import cats.syntax.option._
import java.nio.charset.StandardCharsets.UTF_8
import js7.base.auth.SessionToken
import js7.base.circeutils.CirceUtils._
import js7.base.problem.Problem
import js7.base.time.ScalaTime._
import js7.base.utils.AutoClosing.autoClosing
import js7.base.utils.Closer.syntax.RichClosersAutoCloseable
import js7.base.utils.HasCloser
import js7.base.web.HttpClient.liftProblem
import js7.base.web.Uri
import js7.common.akkahttp.CirceJsonOrYamlSupport
import js7.common.akkahttp.StandardMarshallers._
import js7.common.akkahttp.web.AkkaWebServer
import js7.common.akkautils.Akkas
import js7.common.akkautils.Akkas.newActorSystem
import js7.common.http.AkkaHttpClient.HttpException
import js7.common.http.AkkaHttpClientTest._
import js7.common.scalautil.MonixUtils.syntax._
import js7.common.utils.FreeTcpPortFinder.findFreeTcpPort
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
final class AkkaHttpClientTest extends AnyFreeSpec with BeforeAndAfterAll with HasCloser
{
  implicit private lazy val actorSystem = newActorSystem("AkkaHttpClientTest")

  override def afterAll() = {
    closer.close()
    // FIXME shutdownAllConnectionPools blocks longer than 99s after "connection refused"
    Akkas.terminateAndWait(actorSystem, 9.s)
    super.afterAll()
  }

  "Without a server" - {
    lazy val httpClient = new AkkaHttpClient {
      protected val actorSystem = AkkaHttpClientTest.this.actorSystem
      protected val baseUri = Uri("https://example.com:9999")
      protected val name = "AkkaHttpClientTest"
      protected def uriPrefixPath = "/PREFIX"
      protected def keyStoreRef = None
      protected def trustStoreRefs = Nil
    }.closeWithCloser

    "toCheckedAgentUri, checkAgentUri and apply, failing" - {
      for ((uri, None) <- Setting) s"$uri" in {
        assert(httpClient.checkAgentUri(uri).isLeft)
        assert(httpClient.toCheckedAgentUri(uri).isLeft)
        implicit val s = Task.pure(none[SessionToken])
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

    "Operations after close are rejected" in {
      httpClient.close()
      val uri = Uri("https://example.com:9999/PREFIX")
      implicit val s = Task.pure(none[SessionToken])
      assert(Await.result(httpClient.get_[HttpResponse](uri).runToFuture.failed, 99.seconds).getMessage ==
        "AkkaHttpClient has been closed: GET https://example.com:9999/PREFIX")
    }
  }

  "With a server" - {
    final case class A(int: Int)
    implicit val aJsonCodec = deriveCodec[A]

    lazy val webServer = {
      val server = AkkaWebServer.forTest {
        import CirceJsonOrYamlSupport.{jsonOrYamlMarshaller, jsonUnmarshaller}
        decodeRequest {
          post {
            entity(as[A]) { a =>
              path("OK") {
                complete(OK -> A(a.int + 1))
              } ~
              path("BAD-REQUEST") {
                complete(BadRequest -> "BAD REQUEST")
              } ~
              path("PROBLEM") {
                complete(BadRequest -> Problem("PROBLEM"))
              } ~
              path("SERVER-ERROR") {
                complete(InternalServerError -> "SERVER ERROR")
              }
            }
          }
        }
      }.closeWithCloser
      server.start() await 99.s
      server
    }

    lazy val httpClient = new AkkaHttpClient {
      protected val actorSystem = AkkaHttpClientTest.this.actorSystem
      protected val baseUri = webServer.localUri
      protected val name = "AkkaHttpClientTest"
      protected def uriPrefixPath = ""
      protected def keyStoreRef = None
      protected def trustStoreRefs = Nil
    }.closeWithCloser

    lazy val uri = webServer.localUri
    implicit val sessionToken: Task[Option[SessionToken]] = Task.pure(None)

    "OK" in {
      assert(httpClient.post(Uri(s"$uri/OK"), A(1)).await(99.s) == A(2))
      assert(liftProblem(httpClient.post(Uri(s"$uri/OK"), A(1))).await(99.s) == Right(A(2)))
    }

    "Bad Request post" in {
      val t = intercept[HttpException](
        httpClient.post(Uri(s"$uri/BAD-REQUEST"), A(1)).await(99.s))
      assert(t.toString == s"""HTTP 400 Bad Request: POST $uri/BAD-REQUEST => "BAD REQUEST"""")
    }

    "Bad Request postDiscardResponse" in {
      val t = intercept[HttpException](
        httpClient.postDiscardResponse(Uri(s"$uri/BAD-REQUEST"), A(1)).await(99.s))
      assert(t.toString == s"""HTTP 400 Bad Request: POST $uri/BAD-REQUEST => "BAD REQUEST"""")
    }

    "Problem" in {
      val t = intercept[HttpException](
        httpClient.post(Uri(s"$uri/PROBLEM"), A(1)).await(99.s))
      assert(t.toString == s"""HTTP 400 Bad Request: POST $uri/PROBLEM => PROBLEM""")

      assert(liftProblem(httpClient.post(Uri(s"$uri/PROBLEM"), A(1))).await(99.s) ==
        Left(Problem("PROBLEM")))
    }

    "Internal Server Error" in {
      val t = intercept[HttpException](
        httpClient.post(Uri(s"$uri/SERVER-ERROR"), A(1)).await(99.s))
      assert(t.toString == s"""HTTP 500 Internal Server Error: POST $uri/SERVER-ERROR => "SERVER ERROR"""")
    }

    "close" in {
      httpClient.close()
      webServer.close()
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
        POST,
        Uri("/URI"),
        HttpResponse(BadRequest, entity = HttpEntity(`application/json`, jsonString.getBytes(UTF_8))),
        jsonString)
      assert(e.problem == Some(problem))
      assert(liftProblem(Task.raiseError(e)).runSyncUnsafe(99.seconds) == Left(problem))
    }

    "HttpException with broken problem" in {
      val jsonString = "{}"
      val e = new HttpException(
        POST,
        Uri("/URI"),
        HttpResponse(BadRequest, entity = HttpEntity(`application/json`, jsonString.getBytes(UTF_8))),
        jsonString)
      assert(e.problem.isEmpty)
      assert(liftProblem(Task.raiseError(e)).failed.runSyncUnsafe(99.seconds) eq e)
    }

    "HttpException with string response" in {
      val e = new HttpException(
        POST,
        Uri("/URI"),
        HttpResponse(BadRequest, `Content-Type`(`text/plain(UTF-8)`) :: Nil),
        "{}")
      assert(liftProblem(Task.raiseError(e)).failed.runSyncUnsafe(99.seconds) eq e)
    }

    "Other exception" in {
      val e = new Exception
      assert(liftProblem(Task.raiseError(e)).failed.runSyncUnsafe(99.seconds) eq e)
    }
  }

  "Connection refused, try two times" - {
    lazy val uri = Uri(s"http://127.0.0.1:${findFreeTcpPort()}")

    def newHttpClient() = new AkkaHttpClient {
      protected val actorSystem = AkkaHttpClientTest.this.actorSystem
      protected val baseUri = uri
      protected val name = "AkkaHttpClientTest"
      protected def uriPrefixPath = "/PREFIX"
      protected def keyStoreRef = None
      protected def trustStoreRefs = Nil
    }
    var duration: FiniteDuration = null
    implicit val sessionToken = Task.pure(none[SessionToken])

    "First call" in {
      autoClosing(newHttpClient()) { httpClient =>
        val since = now
        val whenGot = httpClient.get_(Uri(s"$uri/PREFIX/TEST")).runToFuture
        val a = Await.ready(whenGot, 99.s - 1.s)
        duration = since.elapsed
        assert(a.value.get.isFailure)
        whenGot.cancel()
      }
    }

    "Second call lets akka-http block for a very long time" in {
      // Akka 2.6.5 blocks on next call after connection failure (why?),
      // so our responseTimeout kicks in.
      // TODO Replace by http4s ?
      autoClosing(newHttpClient()) { httpClient =>
        val whenGot = httpClient.get_(Uri(s"$uri/PREFIX/TEST")).runToFuture
        intercept[TimeoutException] {
          Await.ready(whenGot, duration + 1.s)
        }
        whenGot.cancel()
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
    Uri("https://example.com:9999/js7/invalid") ->
      None,
    Uri("https://example.com:9999/js7/invalid/api") ->
      None,
    Uri("//example.com:9999/PREFIX/api") ->
      None,
    Uri("https:/PREFIX/api") ->
      None,
    Uri("/js7/invalid") ->
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
