package com.sos.jobscheduler.master.client

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.coding.Gzip
import akka.http.scaladsl.marshalling.Marshal
import akka.http.scaladsl.model.HttpMethods.{GET, POST}
import akka.http.scaladsl.model.MediaTypes.`application/json`
import akka.http.scaladsl.model.headers.CacheDirectives.{`no-cache`, `no-store`}
import akka.http.scaladsl.model.headers.{Accept, `Cache-Control`}
import akka.http.scaladsl.model.{HttpRequest, RequestEntity, StatusCode}
import akka.http.scaladsl.unmarshalling.{FromResponseUnmarshaller, Unmarshal}
import akka.stream.ActorMaterializer
import com.sos.jobscheduler.base.utils.Strings.TruncatedString
import com.sos.jobscheduler.common.CirceJsonSupport._
import com.sos.jobscheduler.common.akkahttp.AkkaHttpClientUtils.RichHttpResponse
import com.sos.jobscheduler.common.akkahttp.AkkaHttpUtils.{decodeResponse, encodeGzip}
import com.sos.jobscheduler.master.client.AkkaHttpClient._
import io.circe.{Decoder, Encoder}
import scala.concurrent.duration.Duration
import scala.concurrent.{ExecutionContext, Future}

/**
  * @author Joacim Zschimmer
  */
trait AkkaHttpClient extends HttpClient {
  protected def actorSystem: ActorSystem
  protected implicit def executionContext: ExecutionContext

  private implicit lazy val materializer = ActorMaterializer()(actorSystem)
  private lazy val http = Http(actorSystem)

  def get[A: Decoder](uri: String, timeout: Duration): Future[A] =
    sendReceive[A](HttpRequest(GET, uri, Accept(`application/json`) :: `Cache-Control`(`no-cache`, `no-store`) :: Nil))

  def post[A: Encoder, B: Decoder](uri: String, data: A): Future[B] =
    (for {
      entity ← Marshal(data).to[RequestEntity]
      result ← sendReceive[B](Gzip.encodeMessage(HttpRequest(POST, uri, Accept(`application/json`) :: Nil, entity)))
    } yield result)

  private def sendReceive[A: FromResponseUnmarshaller](request: HttpRequest): Future[A] =
    for {
      httpResponse ← http.singleRequest(encodeGzip(request))
      response ←
        if (httpResponse.status.isSuccess)
          Unmarshal(decodeResponse(httpResponse)).to[A]
        else
          for (message ← decodeResponse(httpResponse).utf8StringFuture) yield
            throw new HttpException(httpResponse.status, request.uri + ": " + message.truncateWithEllipsis(ErrorMessageLengthMaximum))
    } yield response
}

object AkkaHttpClient {
  private val ErrorMessageLengthMaximum = 10000

  final class Standard(protected val actorSystem: ActorSystem)(implicit protected val executionContext: ExecutionContext)
  extends AkkaHttpClient

  final class StandAlone extends AkkaHttpClient with AutoCloseable {
    protected val actorSystem = ActorSystem("AkkaHttpClient")
    protected val executionContext = actorSystem.dispatcher

    def close() = actorSystem.terminate()
  }

  final class HttpException(val status: StatusCode, message: String) extends RuntimeException(s"$status: $message".trim)
}
