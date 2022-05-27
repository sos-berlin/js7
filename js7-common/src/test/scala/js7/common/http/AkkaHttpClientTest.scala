package js7.common.http

import akka.http.scaladsl.model.ContentTypes.{`application/json`, `text/plain(UTF-8)`}
import akka.http.scaladsl.model.HttpMethods.POST
import akka.http.scaladsl.model.StatusCodes.{BadRequest, InternalServerError, OK}
import akka.http.scaladsl.model.headers.`Content-Type`
import akka.http.scaladsl.model.{HttpEntity, HttpResponse}
import akka.http.scaladsl.server.Directives._
import akka.util.ByteString
import cats.syntax.option._
import io.circe.generic.semiauto.deriveCodec
import java.nio.charset.StandardCharsets.UTF_8
import js7.base.auth.SessionToken
import js7.base.circeutils.CirceUtils._
import js7.base.configutils.Configs.HoconStringInterpolator
import js7.base.io.https.HttpsConfig
import js7.base.log.CorrelId
import js7.base.problem.Problem
import js7.base.thread.MonixBlocking.syntax._
import js7.base.time.ScalaTime._
import js7.base.utils.AutoClosing.autoClosing
import js7.base.utils.Closer.syntax.RichClosersAutoCloseable
import js7.base.utils.HasCloser
import js7.base.web.HttpClient.liftProblem
import js7.base.web.Uri
import js7.common.akkahttp.CirceJsonSupport
import js7.common.akkahttp.StandardMarshallers._
import js7.common.akkahttp.web.AkkaWebServer
import js7.common.akkautils.Akkas
import js7.common.akkautils.Akkas.newActorSystem
import js7.common.http.AkkaHttpClient.{HttpException, `x-js7-correlation-id`, toPrettyProblem}
import js7.common.http.AkkaHttpClientTest._
import js7.common.http.JsonStreamingSupport.`application/x-ndjson`
import js7.common.http.StreamingSupport.AkkaObservable
import js7.common.utils.FreeTcpPortFinder.findFreeTcpPort
import monix.eval.Task
import monix.execution.Scheduler.Implicits.traced
import monix.reactive.Observable
import org.scalatest.BeforeAndAfterAll
import org.scalatest.freespec.AnyFreeSpec
import scala.concurrent.Await
import scala.concurrent.duration.Deadline.now
import scala.concurrent.duration._
import scala.util.{Failure, Success}

/**
  * @author Joacim Zschimmer
  */
final class AkkaHttpClientTest extends AnyFreeSpec with BeforeAndAfterAll with HasCloser
{
  implicit private lazy val actorSystem = newActorSystem("AkkaHttpClientTest",
    config"""akka.http.client.idle-timeout = 2s""")

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
      protected def httpsConfig = HttpsConfig.empty
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

    //"Operations after close are rejected" in {
    //  httpClient.close()
    //  val uri = Uri("https://example.com:9999/PREFIX")
    //  implicit val s = Task.pure(none[SessionToken])
    //  assert(Await.result(httpClient.get_[HttpResponse](uri).runToFuture.failed, 99.seconds).getMessage ==
    //    "AkkaHttpClient has been closed: GET https://example.com:9999/PREFIX")
    //}
  }

  "With a server" - {
    implicit val aJsonCodec = deriveCodec[A]
    lazy val webServer = {
      val server = AkkaWebServer.forTest() {
        decodeRequest {
          import CirceJsonSupport.jsonMarshaller
          post {
            import CirceJsonSupport.jsonUnmarshaller
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
          } ~
            get {
              path("STREAM") {
                val source = Observable("ONE\n", "TWO\n")
                  .map(ByteString(_))
                  .toAkkaSourceForHttpResponse
                complete(HttpEntity(`application/x-ndjson`, source))
              } ~
              path("IDLE-TIMEOUT") {
                val source = Observable(ByteString("IDLE-TIMEOUT\n"))
                  .delayExecution(5.s)
                  .toAkkaSourceForHttpResponse
                complete(HttpEntity(`application/x-ndjson`, source))
              }
            }
        }
      }.closeWithCloser
      server.start await 99.s
      server
    }

    lazy val httpClient = new AkkaHttpClient {
      protected val actorSystem = AkkaHttpClientTest.this.actorSystem
      protected val baseUri = webServer.localUri
      protected val name = "AkkaHttpClientTest"
      protected def uriPrefixPath = ""
      protected def httpsConfig = HttpsConfig.empty
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

    "getRawLinesObservable" in {
      val result = Observable
        .fromTask(httpClient.getRawLinesObservable(Uri(s"$uri/STREAM")))
        .flatten
        .toListL
        .await(99.s)
        .map(_.utf8String)
      assert(result == List("ONE\n", "TWO\n"))
    }

    "getRawLinesObservable: idle-timeout yield an empty observable" in {
      // AkkaHttpClient converts the TcpIdleTimeoutException to the empty Observable
      val result = Observable
        .fromTask(httpClient.getRawLinesObservable(Uri(s"$uri/IDLE-TIMEOUT")))
        .flatten
        .toListL
        .await(99.s)
      assert(result.isEmpty)
    }

    "close" in {
      httpClient.close()
      webServer.stop().await(99.s)
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
      assert(e.getMessage == "HTTP 400 Bad Request: POST /URI => {}")
      assert(liftProblem(Task.raiseError(e)).runSyncUnsafe(99.seconds) == Left(Problem(e.getMessage)))
    }

    "HttpException with string response" in {
      val e = new HttpException(
        POST,
        Uri("/URI"),
        HttpResponse(BadRequest, `Content-Type`(`text/plain(UTF-8)`) :: Nil),
        "{}")
      assert(e.getMessage == "HTTP 400 Bad Request: POST /URI => {}")
      assert(liftProblem(Task.raiseError(e)).runSyncUnsafe(99.seconds) == Left(Problem(e.getMessage)))
    }

    "Other exception" in {
      val e = new Exception
      assert(liftProblem(Task.raiseError(e)).failed.runSyncUnsafe(99.seconds) eq e)
    }
  }

  "toPrettyProblem" in {
    val t = new akka.stream.ConnectionException(
      "Tcp command [Connect(agent-1/<unresolved>:4443,None,List(),Some(10 seconds),true)] failed because of java.net.ConnectException: Connection refused")
      .initCause(new java.net.SocketException("Connection refused"))
    assert(toPrettyProblem(Problem.fromThrowable(t)) == Problem("TCP Connect agent-1:4443: Connection refused"))
  }

  "Connection refused, try two times" - {
    lazy val uri = Uri(s"http://127.0.0.1:${findFreeTcpPort()}")

    def newHttpClient() = new AkkaHttpClient {
      protected val actorSystem = AkkaHttpClientTest.this.actorSystem
      protected val baseUri = uri
      protected val name = "AkkaHttpClientTest"
      protected def uriPrefixPath = "/PREFIX"
      protected def httpsConfig = HttpsConfig.empty
    }
    implicit val sessionToken = Task.pure(none[SessionToken])

    "First call" in {
      autoClosing(newHttpClient()) { httpClient =>
        val since = now
        val whenGot = httpClient.get_(Uri(s"$uri/PREFIX/TEST")).runToFuture
        val a = Await.ready(whenGot, 99.s - 1.s)
        assert(a.value.get.isFailure)
      }
    }

    "Second call lets akka-http not block for a very long time" in {
      // Akka 2.6.5 or our AkkaHttpClient blocked on next call after connection failure.
      // This does not occur anymore!
      autoClosing(newHttpClient()) { httpClient =>
        val whenGot = httpClient.get_(Uri(s"$uri/PREFIX/TEST")).runToFuture
        val a = Await.ready(whenGot, 99.s - 1.s)
        assert(a.value.get.isFailure)
      }
    }
  }

  "Header x-js7-correlation-id uses ASCII" in {
    val h = `x-js7-correlation-id`(CorrelId("_CORRELñ"))
    val asciiString = "_CORREL-"

    assert(h.value == asciiString)
    assert(`x-js7-correlation-id`.parse(asciiString) == Success(h))

    assert(`x-js7-correlation-id`.parse("_CORRELñX") == Failure(Problem("Invalid CorrelId")
      .throwable))
    assert(`x-js7-correlation-id`.parse("_CORREL?") == Failure(Problem("Invalid CorrelId")
      .throwable))
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

  private final case class A(int: Int)
}
