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
import com.sos.jobscheduler.common.http.AkkaHttpUtils.{decodeResponse, encodeGzip}
import com.sos.jobscheduler.common.http.CirceJsonSupport._
import io.circe.{Decoder, Encoder}
import monix.eval.Task
import monix.eval.Task.deferFuture
import scala.concurrent.duration._

/**
  * @author Joacim Zschimmer
  */
trait AkkaHttpClient extends AutoCloseable with HttpClient
{
  protected def actorSystem: ActorSystem

  protected def userAndPassword: Option[UserAndPassword]

  implicit lazy val materializer = ActorMaterializer()(actorSystem)
  private lazy val http = Http(actorSystem)

  protected def httpsConnectionContext: HttpsConnectionContext = http.defaultClientHttpsContext

  def close() = materializer.shutdown()

  def get[A: Decoder](uri: String, timeout: Duration): Task[A] =
    get(Uri(uri), timeout)

  /** HTTP Get with Accept: application/json. */
  def get[A: Decoder](uri: Uri, timeout: Duration): Task[A] =
    get_[A](uri, Accept(`application/json`) :: Nil)

  def get_[A: FromResponseUnmarshaller](uri: Uri, headers: List[HttpHeader] = Nil): Task[A] =
    sendReceive(HttpRequest(GET, uri, headers ::: `Cache-Control`(`no-cache`, `no-store`) :: Nil))
      .flatMap(unmarshal[A](uri))

  def post[A: Encoder, B: Decoder](uri: String, data: A): Task[B] =
    post[A, B](Uri(uri), data)

  def post[A: Encoder, B: Decoder](uri: Uri, data: A): Task[B] =
    post_[A](uri, Accept(`application/json`) :: Nil, data)
      .flatMap(unmarshal[B](uri))

  def postDiscardResponse[A: Encoder](uri: String, data: A): Task[Int] =
    post_[A](uri, Accept(`application/json`) :: Nil, data) map { response ⇒
      response.entity.discardBytes()
      response.status.intValue
    }

  def post_[A: Encoder](uri: Uri, headers: List[HttpHeader], data: A): Task[HttpResponse] =
    for {
      entity ← Task.deferFutureAction(implicit scheduler ⇒ Marshal(data).to[RequestEntity])
      response ← sendReceive(Gzip.encodeMessage(HttpRequest(POST, uri, headers, entity)))
    } yield response

  private def sendReceive(request: HttpRequest): Task[HttpResponse] = {
    val authentication = userAndPassword map (o ⇒ Authorization(BasicHttpCredentials(o.userId.string, o.password.string)))
    val myRequest = encodeGzip(request.withHeaders(authentication ++: request.headers))
    deferFuture(http.singleRequest(myRequest, httpsConnectionContext))
      .map(decodeResponse)
  }

  private def unmarshal[A: FromResponseUnmarshaller](uri: Uri)(httpResponse: HttpResponse): Task[A] =
    if (httpResponse.status.isSuccess)
      deferFuture(Unmarshal(httpResponse).to[A])
    else
      for (entity ← deferFuture(httpResponse.entity.toStrict(FailureTimeout))) yield
        throw new HttpException(httpResponse.status, uri, entity.data.utf8String.truncateWithEllipsis(ErrorMessageLengthMaximum))
}

object AkkaHttpClient {
  private val ErrorMessageLengthMaximum = 10000
  private val FailureTimeout = 30.seconds

  final class HttpException private[AkkaHttpClient](val status: StatusCode, val uri: Uri, val dataAsString: String)
  extends RuntimeException(s"$status: $uri: $dataAsString".trim)
}
