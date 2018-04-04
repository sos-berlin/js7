package com.sos.jobscheduler.common.http

import akka.actor.ActorSystem
import akka.http.scaladsl.coding.Gzip
import akka.http.scaladsl.marshalling.Marshal
import akka.http.scaladsl.model.HttpMethods.{GET, POST}
import akka.http.scaladsl.model.MediaTypes.`application/json`
import akka.http.scaladsl.model.headers.CacheDirectives.{`no-cache`, `no-store`}
import akka.http.scaladsl.model.headers.{Accept, Authorization, BasicHttpCredentials, `Cache-Control`}
import akka.http.scaladsl.model.{HttpHeader, HttpRequest, HttpResponse, RequestEntity, StatusCode, Uri}
import akka.http.scaladsl.unmarshalling.{FromResponseUnmarshaller, Unmarshal}
import akka.http.scaladsl.{Http, HttpsConnectionContext}
import akka.stream.ActorMaterializer
import com.sos.jobscheduler.base.auth.UserAndPassword
import com.sos.jobscheduler.base.utils.Strings.RichString
import com.sos.jobscheduler.common.http.AkkaHttpClient._
import com.sos.jobscheduler.common.http.AkkaHttpUtils.{RichHttpResponse, decodeResponse, encodeGzip}
import com.sos.jobscheduler.common.http.CirceJsonSupport._
import io.circe.{Decoder, Encoder}
import scala.concurrent.duration.Duration
import scala.concurrent.{ExecutionContext, Future}

/**
  * @author Joacim Zschimmer
  */
trait AkkaHttpClient extends AutoCloseable with HttpClient
{
  protected def actorSystem: ActorSystem

  protected implicit def executionContext: ExecutionContext

  protected def userAndPassword: Option[UserAndPassword]

  implicit lazy val materializer = ActorMaterializer()(actorSystem)
  private lazy val http = Http(actorSystem)

  protected def httpsConnectionContext: HttpsConnectionContext = http.defaultClientHttpsContext

  def close() = materializer.shutdown()

  def get[A: Decoder](uri: String, timeout: Duration): Future[A] =
    get(Uri(uri), timeout)

  def get[A: Decoder](uri: Uri, timeout: Duration): Future[A] =
    sendReceive(HttpRequest(GET, uri, Accept(`application/json`) :: `Cache-Control`(`no-cache`, `no-store`) :: Nil))
      .flatMap(decode[A](uri))

  def post[A: Encoder, B: Decoder](uri: String, data: A): Future[B] =
    post[A, B](Uri(uri), data)

  def post[A: Encoder, B: Decoder](uri: Uri, data: A): Future[B] =
    post_[A](uri, data, Accept(`application/json`) :: Nil)
      .flatMap(decode[B](uri))

  def postIgnoreResponse[A: Encoder](uri: String, data: A): Future[Int] =
    post_[A](uri, data) map (_.status.intValue)

  def post_[A: Encoder](uri: Uri, data: A, headers: List[HttpHeader] = Nil): Future[HttpResponse] =
    for {
      entity ← Marshal(data).to[RequestEntity]
      response ← sendReceive(Gzip.encodeMessage(HttpRequest(POST, uri, headers, entity)))
    } yield response

  private def sendReceive(request: HttpRequest): Future[HttpResponse] = {
    val authentication = userAndPassword map (o ⇒ Authorization(BasicHttpCredentials(o.userId.string, o.password.string)))
    val myRequest = encodeGzip(request.withHeaders(
      Vector(Accept(`application/json`), `Cache-Control`(`no-cache`, `no-store`)) ++ authentication ++ request.headers))
    http.singleRequest(myRequest, httpsConnectionContext) map decodeResponse
  }

  private def decode[A: FromResponseUnmarshaller](uri: Uri)(httpResponse: HttpResponse): Future[A] =
    if (httpResponse.status.isSuccess)
      Unmarshal(httpResponse).to[A]
    else
      for (message ← httpResponse.utf8StringFuture) yield
        throw new HttpException(httpResponse.status, uri, message.truncateWithEllipsis(ErrorMessageLengthMaximum))
}

object AkkaHttpClient {
  private val ErrorMessageLengthMaximum = 10000

  //final class Standard(protected val actorSystem: ActorSystem)(implicit protected val executionContext: ExecutionContext)
  //extends AkkaHttpClient

  final class HttpException(val status: StatusCode, val uri: Uri, val message: String)
  extends RuntimeException(s"$status: $uri: $message".trim)
}
