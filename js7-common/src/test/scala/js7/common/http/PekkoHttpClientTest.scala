package js7.common.http

import cats.syntax.option.*
import io.circe.Codec
import io.circe.generic.semiauto.deriveCodec
import java.nio.charset.StandardCharsets.UTF_8
import js7.base.auth.SessionToken
import js7.base.circeutils.CirceUtils.*
import js7.base.configutils.Configs.HoconStringInterpolator
import js7.base.io.https.HttpsConfig
import js7.base.log.CorrelId
import js7.base.problem.Problem
import js7.base.test.OurTestSuite
import js7.base.thread.MonixBlocking.syntax.*
import js7.base.time.ScalaTime.*
import js7.base.utils.AutoClosing.autoClosing
import js7.base.utils.CatsUtils.syntax.RichResource
import js7.base.utils.Closer.syntax.RichClosersAutoCloseable
import js7.base.utils.{Allocated, HasCloser}
import js7.base.web.HttpClient.liftProblem
import js7.base.web.Uri
import js7.common.http.JsonStreamingSupport.`application/x-ndjson`
import js7.common.http.PekkoHttpClient.{HttpException, `x-js7-correlation-id`, `x-js7-request-id`, toPrettyProblem}
import js7.common.http.PekkoHttpClientTest.*
import js7.common.http.StreamingSupport.PekkoObservable
import js7.common.pekkohttp.CirceJsonSupport
import js7.common.pekkohttp.web.PekkoWebServer
import js7.common.pekkoutils.Pekkos
import js7.common.pekkoutils.Pekkos.newActorSystem
import js7.common.utils.FreeTcpPortFinder.findFreeLocalUri
import monix.eval.Task
import monix.execution.Scheduler.Implicits.traced
import monix.reactive.Observable
import org.apache.pekko.actor.ActorSystem
import org.apache.pekko.http.scaladsl.model.ContentTypes.{`application/json`, `text/plain(UTF-8)`}
import org.apache.pekko.http.scaladsl.model.HttpMethods.POST
import org.apache.pekko.http.scaladsl.model.StatusCodes.{BadRequest, InternalServerError, OK}
import org.apache.pekko.http.scaladsl.model.headers.`Content-Type`
import org.apache.pekko.http.scaladsl.model.{HttpEntity, HttpResponse}
import org.apache.pekko.http.scaladsl.server.Directives.*
import org.apache.pekko.util.ByteString
import org.scalatest.BeforeAndAfterAll
import scala.concurrent.Await
import scala.concurrent.duration.*
import scala.concurrent.duration.Deadline.now
import scala.util.{Failure, Success}

/**
  * @author Joacim Zschimmer
  */
final class PekkoHttpClientTest extends OurTestSuite with BeforeAndAfterAll with HasCloser
{
  implicit private lazy val actorSystem: ActorSystem =
    newActorSystem("PekkoHttpClientTest", config"""pekko.http.client.idle-timeout = 2s""")

  override def afterAll() = {
    closer.close()
    // TODO shutdownAllConnectionPools blocks longer than 99s after "connection refused"
    Pekkos.terminateAndWait(actorSystem, 9.s)
    super.afterAll()
  }

  "Without a server" - {
    lazy val httpClient = new PekkoHttpClient {
      protected val actorSystem = PekkoHttpClientTest.this.actorSystem
      protected val baseUri = Uri("https://example.com:9999")
      protected val name = "PekkoHttpClientTest"
      protected def uriPrefixPath = "/PREFIX"
      protected def httpsConfig = HttpsConfig.empty
    }.closeWithCloser

    "toCheckedAgentUri, checkAgentUri and apply, failing" - {
      for case (uri, None) <- Setting do s"$uri" in {
        assert(httpClient.checkAgentUri(uri).isLeft)
        assert(httpClient.toCheckedAgentUri(uri).isLeft)
        implicit val s = Task.pure(none[SessionToken])
        assert(Await.result(httpClient.get_[HttpResponse](uri).runToFuture.failed, 99.seconds).getMessage
          contains "does not match")
      }
    }

    "normalizeAgentUri" - {
      for case (uri, Some(converted)) <- Setting do s"$uri" in {
        assert(httpClient.normalizeAgentUri(uri) == converted)
        assert(httpClient.toCheckedAgentUri(uri) == Right(converted))
      }
    }

    //"Operations after close are rejected" in {
    //  httpClient.close()
    //  val uri = Uri("https://example.com:9999/PREFIX")
    //  implicit val s = Task.pure(none[SessionToken])
    //  assert(Await.result(httpClient.get_[HttpResponse](uri).runToFuture.failed, 99.seconds).getMessage ==
    //    "PekkoHttpClient has been closed: GET https://example.com:9999/PREFIX")
    //}
  }

  "With a server" - {
    implicit val aJsonCodec: Codec.AsObject[A] = deriveCodec
    lazy val allocatedWebServer: Allocated[Task, PekkoWebServer] = PekkoWebServer
      .testResource() {
        decodeRequest {
          import CirceJsonSupport.jsonMarshaller
          post {
            import CirceJsonSupport.jsonUnmarshaller
            entity(as[A])(a =>
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
              })
          } ~
            get {
              path("STREAM") {
                val source = Observable("ONE\n", "TWO\n")
                  .map(ByteString(_))
                  .toPekkoSourceForHttpResponse
                complete(HttpEntity(`application/x-ndjson`, source))
              } ~
              path("IDLE-TIMEOUT") {
                val source = Observable(ByteString("IDLE-TIMEOUT\n"))
                  .delayExecution(5.s)
                  .toPekkoSourceForHttpResponse
                complete(HttpEntity(`application/x-ndjson`, source))
              }
            }
        }
      }
      .toAllocated
      .await(99.s)

    lazy val httpClient = new PekkoHttpClient {
      protected val actorSystem = PekkoHttpClientTest.this.actorSystem
      protected val baseUri = allocatedWebServer.allocatedThing.localUri
      protected val name = "PekkoHttpClientTest"
      protected def uriPrefixPath = ""
      protected def httpsConfig = HttpsConfig.empty
    }.closeWithCloser

    lazy val uri = allocatedWebServer.allocatedThing.localUri
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
      // PekkoHttpClient converts the TcpIdleTimeoutException to the empty Observable
      val result = Observable
        .fromTask(httpClient.getRawLinesObservable(Uri(s"$uri/IDLE-TIMEOUT")))
        .flatten
        .toListL
        .await(99.s)
      assert(result.isEmpty)
    }

    "close" in {
      httpClient.close()
      allocatedWebServer.release.await(99.s)
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
    val t = new org.apache.pekko.stream.ConnectionException(
      "Tcp command [Connect(agent-1/<unresolved>:4443,None,List(),Some(10 seconds),true)] failed because of java.net.ConnectException: Connection refused")
      .initCause(new java.net.SocketException("Connection refused"))
    assert(toPrettyProblem(Problem.fromThrowable(t)) == Problem("TCP Connect agent-1:4443: Connection refused"))
  }

  "Connection refused, try two times" - {
    lazy val uri = findFreeLocalUri()

    def newHttpClient() = new PekkoHttpClient {
      protected val actorSystem = PekkoHttpClientTest.this.actorSystem
      protected val baseUri = uri
      protected val name = "PekkoHttpClientTest"
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

    "Second call lets pekko-http not block for a very long time" in {
      // Akka 2.6.5 or our PekkoHttpClient blocked on next call after connection failure.
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

  "Header x-js7-request-id" in {
    val n = 111222333444555666L
    val h = `x-js7-request-id`(n)
    val string = s"#$n"
    assert(h.value == string)
    assert(`x-js7-request-id`.parse(string) == Success(h))
    assert(`x-js7-request-id`.parse("123").isFailure)
    assert(`x-js7-request-id`.parse("#123X").isFailure)
  }
}


object PekkoHttpClientTest:

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
