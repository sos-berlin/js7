package com.sos.jobscheduler.common.http

import akka.actor.ActorSystem
import akka.http.scaladsl.marshalling.Marshal
import akka.http.scaladsl.model.HttpMethods.{GET, POST}
import akka.http.scaladsl.model.MediaTypes.`application/json`
import akka.http.scaladsl.model.StatusCodes.{Forbidden, Unauthorized}
import akka.http.scaladsl.model.headers.CacheDirectives.{`no-cache`, `no-store`}
import akka.http.scaladsl.model.headers.{Accept, RawHeader, `Cache-Control`}
import akka.http.scaladsl.model.{HttpHeader, HttpRequest, HttpResponse, RequestEntity, StatusCode, Uri}
import akka.http.scaladsl.unmarshalling.{FromResponseUnmarshaller, Unmarshal}
import akka.http.scaladsl.{Http, HttpsConnectionContext}
import akka.stream.ActorMaterializer
import cats.data.Validated.{Invalid, Valid}
import com.sos.jobscheduler.base.auth.SessionToken
import com.sos.jobscheduler.base.problem.{Checked, Problem}
import com.sos.jobscheduler.base.session.HasSessionToken
import com.sos.jobscheduler.base.utils.Lazy
import com.sos.jobscheduler.base.utils.ScalaUtils.RichJavaClass
import com.sos.jobscheduler.base.utils.Strings.RichString
import com.sos.jobscheduler.base.web.HttpClient
import com.sos.jobscheduler.common.http.AkkaHttpClient._
import com.sos.jobscheduler.common.http.AkkaHttpUtils.{decodeResponse, encodeGzip}
import com.sos.jobscheduler.common.http.CirceJsonSupport._
import com.typesafe.scalalogging.Logger
import io.circe.{Decoder, Encoder}
import monix.eval.Task
import scala.concurrent.duration._
import scala.reflect.ClassTag
import scala.util.Failure

/**
  * @author Joacim Zschimmer
  */
trait AkkaHttpClient extends AutoCloseable with HttpClient with HasSessionToken
{
  protected def actorSystem: ActorSystem
  protected def baseUri: Uri
  protected def uriPrefixPath: String
  protected def standardHeaders: List[HttpHeader] = Nil

  private lazy val http = Http(actorSystem)
  private val materializerLazy = Lazy(ActorMaterializer()(actorSystem))

  implicit final def materializer = materializerLazy()

  protected def httpsConnectionContextOption: Option[HttpsConnectionContext] = None

  private lazy val httpsConnectionContext = httpsConnectionContextOption getOrElse http.defaultClientHttpsContext

  def close() = for (o ← materializerLazy) o.shutdown()

  def get[A: Decoder](uri: String, timeout: Duration): Task[A] =
    get[A](uri, timeout, Nil)

  /** HTTP Get with Accept: application/json. */
  def get[A: Decoder](uri: Uri): Task[A] =
    get[A](uri, Duration.Inf, Nil)

  /** HTTP Get with Accept: application/json. */
  def get[A: Decoder](uri: Uri, timeout: Duration): Task[A] =
    get[A](uri, timeout, Nil)

  /** HTTP Get with Accept: application/json. */
  def get[A: Decoder](uri: Uri, headers: List[HttpHeader]): Task[A] =
    get[A](uri, Duration.Inf, headers)

  /** HTTP Get with Accept: application/json. */
  def get[A: Decoder](uri: Uri, timeout: Duration, headers: List[HttpHeader]): Task[A] =
    get_[A](uri, headers ::: Accept(`application/json`) :: Nil)

  def get_[A: FromResponseUnmarshaller](uri: Uri, headers: List[HttpHeader] = Nil): Task[A] =
    sendReceive(HttpRequest(GET, uri, headers ::: `Cache-Control`(`no-cache`, `no-store`) :: Nil))
      .flatMap(unmarshal[A](uri))

  def post[A: Encoder, B: Decoder](uri: String, data: A, suppressSessionToken: Boolean): Task[B] =
    post2[A, B](Uri(uri), data, Nil, suppressSessionToken = suppressSessionToken)

  def postWithHeaders[A: Encoder, B: Decoder](uri: Uri, data: A, headers: List[HttpHeader]): Task[B] =
    post2[A, B](uri, data, headers, suppressSessionToken = false)

  private def post2[A: Encoder, B: Decoder](uri: Uri, data: A, headers: List[HttpHeader], suppressSessionToken: Boolean): Task[B] =
    post_[A](uri, data, headers ::: Accept(`application/json`) :: Nil, suppressSessionToken = suppressSessionToken)
      .flatMap(unmarshal[B](uri))

  def postDiscardResponse[A: Encoder](uri: String, data: A): Task[Int] =
    post_[A](uri, data, Accept(`application/json`) :: Nil) map { response ⇒
      response.discardEntityBytes()
      response.status.intValue
    }

  def post_[A: Encoder](uri: Uri, data: A, headers: List[HttpHeader], suppressSessionToken: Boolean = false): Task[HttpResponse] =
    for {
      entity ← Task.deferFutureAction(implicit scheduler ⇒ Marshal(data).to[RequestEntity])
      response ← sendReceive(
        HttpRequest(POST, uri, headers, entity),
        suppressSessionToken = suppressSessionToken,
        logData = Some(data.getClass.scalaName stripPrefix "com.sos.jobscheduler."))
    } yield response

  def postRaw(uri: Uri, headers: List[HttpHeader], entity: RequestEntity): Task[HttpResponse] =
    sendReceive(HttpRequest(POST, uri, headers, entity))

  final def sendReceive(request: HttpRequest, suppressSessionToken: Boolean = false, logData: ⇒ Option[String] = None): Task[HttpResponse] =
    withCheckedAgentUri(request) { request ⇒
      val headers = if (suppressSessionToken) None else sessionToken map (token ⇒ RawHeader(SessionToken.HeaderName, token.secret.string))
      val req = encodeGzip(request.withHeaders(headers ++: request.headers ++: standardHeaders))
      Task.deferFuture {
        logRequest(req, logData)
        val httpsContext = if (request.uri.scheme == "https") httpsConnectionContext else http.defaultClientHttpsContext
        http.singleRequest(req, httpsContext)
      } map decodeResponse  // Decompress
    }

  private def logRequest(request: HttpRequest, logData: ⇒ Option[String]): Unit =
    if (logger.underlying.isDebugEnabled) {
      val b = new StringBuilder(200)
      b.append(request.method.value)
      b.append(' ')
      b.append(request.uri)
      if (!request.entity.isKnownEmpty) {
        b.append(": ")
        b.append(request.entity.contentType)
        for (o ← logData) {
          b.append(' ')
          b.append(o)  // Do not show JSON because it may contain visible credentials
        }
      }
      logger.debug(b.toString)
    }

  private def unmarshal[A: FromResponseUnmarshaller](uri: Uri)(httpResponse: HttpResponse): Task[A] =
    if (httpResponse.status.isSuccess)
      Task.fromFuture/*consume only once*/(Unmarshal(httpResponse).to[A])
    else
      for (entity ← Task.fromFuture/*consume only once*/(httpResponse.entity.toStrict(FailureTimeout))) yield
        throw new HttpException(httpResponse, uri, entity.data.utf8String.truncateWithEllipsis(ErrorMessageLengthMaximum))

  private def withCheckedAgentUri[A](request: HttpRequest)(body: HttpRequest ⇒ Task[A]): Task[A] =
    toCheckedAgentUri(request.uri) match {
      case Valid(uri) ⇒ body(request.copy(uri = uri))
      case Invalid(problem) ⇒ Task.fromTry(Failure(problem.throwable))
    }

  private[http] final def toCheckedAgentUri(uri: Uri): Checked[Uri] =
    checkAgentUri(normalizeAgentUri(uri))

  private[http] final def normalizeAgentUri(uri: Uri): Uri = {
    val Uri(scheme, authority, path, query, fragment) = uri
    if (scheme.isEmpty && authority.isEmpty)
      Uri(baseUri.scheme, baseUri.authority, path, query, fragment)
    else
      uri
  }

  /** Checks `uri` againts `baseUri` - scheme and authority must be equal. */
  private[http] def checkAgentUri(uri: Uri): Checked[Uri] =
    if (uri.scheme == baseUri.scheme &&
      uri.authority == baseUri.authority &&
       uri.path.toString.startsWith(uriPrefixPath))
      Valid(uri)
    else
      Invalid(Problem(s"URI '$uri' does not match $baseUri$uriPrefixPath"))

  override def toString = s"AkkaHttpClient($baseUri)"
}

object AkkaHttpClient {
  private val ErrorMessageLengthMaximum = 10000
  private val FailureTimeout = 30.seconds
  private val logger = Logger(getClass.getName stripSuffix "$" replaceFirst("^com[.]sos[.]jobscheduler", "jobscheduler"))  // TODO Use Logger adapter (unreachable in module common)

  def sessionMayBeLost(t: Throwable): Boolean =
    t match {
      case t: HttpException if t.status == Unauthorized || t.status == Forbidden ⇒ true
      case _ ⇒ false
    }

  final class HttpException private[AkkaHttpClient](httpResponse: HttpResponse, val uri: Uri, val dataAsString: String)
  extends RuntimeException(s"${httpResponse.status}: $uri: $dataAsString".trim) {
    // Don't publish httpResponse because its entity stream has already been consumed for dataAsString
    def status: StatusCode = httpResponse.status

    def header[A >: Null <: HttpHeader: ClassTag]: Option[A] = httpResponse.header[A]
  }
}
