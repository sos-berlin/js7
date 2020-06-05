package js7.common.http

import akka.actor.ActorSystem
import akka.http.scaladsl.marshalling.Marshal
import akka.http.scaladsl.model.ContentTypes.`text/plain(UTF-8)`
import akka.http.scaladsl.model.HttpMethods.{GET, POST}
import akka.http.scaladsl.model.MediaTypes.`application/json`
import akka.http.scaladsl.model.StatusCodes.{Forbidden, Unauthorized}
import akka.http.scaladsl.model.headers.CacheDirectives.{`no-cache`, `no-store`}
import akka.http.scaladsl.model.headers.{Accept, CustomHeader, RawHeader, `Cache-Control`}
import akka.http.scaladsl.model.{ContentTypes, HttpEntity, HttpHeader, HttpMethod, HttpRequest, HttpResponse, RequestEntity, StatusCode, StatusCodes, Uri => AkkaUri}
import akka.http.scaladsl.unmarshalling.{FromResponseUnmarshaller, Unmarshal}
import akka.http.scaladsl.{Http, HttpsConnectionContext}
import akka.stream.ActorMaterializer
import akka.util.ByteString
import cats.effect.{ExitCase, Resource}
import js7.base.auth.SessionToken
import js7.base.circeutils.CirceUtils.RichCirceString
import js7.base.exceptions.HasIsIgnorableStackTrace
import js7.base.problem.Checked._
import js7.base.problem.{Checked, Problem}
import js7.base.time.ScalaTime._
import js7.base.utils.MonixAntiBlocking.executeOn
import js7.base.utils.ScalaUtils._
import js7.base.utils.ScalazStyle._
import js7.base.utils.Strings.RichString
import js7.base.utils.{ByteVectorToLinesObservable, Lazy}
import js7.base.web.{HttpClient, Uri}
import js7.common.akkahttp.https.AkkaHttps.loadHttpsConnectionContext
import js7.common.akkahttp.https.{KeyStoreRef, TrustStoreRef}
import js7.common.http.AkkaHttpClient._
import js7.common.http.AkkaHttpUtils.{RichAkkaAsUri, RichAkkaUri, decodeResponse, encodeGzip}
import js7.common.http.CirceJsonSupport._
import js7.common.http.JsonStreamingSupport.StreamingJsonHeaders
import js7.common.http.StreamingSupport._
import js7.common.scalautil.Logger
import io.circe.{Decoder, Encoder}
import java.util.Locale
import monix.eval.Task
import monix.execution.Cancelable
import monix.execution.atomic.AtomicLong
import monix.reactive.Observable
import scala.concurrent.Future
import scala.concurrent.duration.Deadline.now
import scala.concurrent.duration._
import scala.reflect.ClassTag
import scala.util.{Failure, Success}
import scodec.bits.ByteVector

/**
  * @author Joacim Zschimmer
  */
trait AkkaHttpClient extends AutoCloseable with HttpClient with HasIsIgnorableStackTrace
{
  protected def actorSystem: ActorSystem
  protected def baseUri: Uri
  protected def uriPrefixPath: String
  protected def name: String
  protected def standardHeaders: List[HttpHeader] = Nil

  private lazy val http = Http(actorSystem)
  private val materializerLazy = Lazy(ActorMaterializer()(actorSystem))
  private lazy val baseAkkaUri = AkkaUri(baseUri.string)
  @volatile private var closed = false
  private val counter = AtomicLong(0)

  implicit final def materializer = materializerLazy()

  protected def keyStoreRef: Option[KeyStoreRef]
  protected def trustStoreRef: Option[TrustStoreRef]

  protected final lazy val httpsConnectionContextOption: Option[HttpsConnectionContext] =
    (keyStoreRef.nonEmpty || trustStoreRef.nonEmpty) ? loadHttpsConnectionContext(keyStoreRef, trustStoreRef)  // TODO None means HttpsConnectionContext? Or empty context?

  private lazy val httpsConnectionContext = httpsConnectionContextOption getOrElse http.defaultClientHttpsContext

  def close() = {
    logger.trace(s"$toString: close")
    closed = true
    for (o <- materializerLazy) {
      logger.debug(s"$toString: ActorMaterializer shutdown")
      o.shutdown()
    }
  }

  def isClosed = closed

  final def getDecodedLinesObservable[A: Decoder](uri: Uri)(implicit s: Task[Option[SessionToken]]) =
    getRawLinesObservable(uri)
      .map(_.map(_.decodeUtf8.orThrow.parseJsonCheckedAs[A].orThrow))

  final def getRawLinesObservable(uri: Uri)(implicit s: Task[Option[SessionToken]]): Task[Observable[ByteVector]] =
    get_[HttpResponse](uri, StreamingJsonHeaders)
      .map(_.entity.withoutSizeLimit.dataBytes
        .toObservable
        .map(o => ByteVector.view(o.toArray))
        .flatMap(new ByteVectorToLinesObservable))

  /** HTTP Get with Accept: application/json. */
  final def get[A: Decoder](uri: Uri)(implicit s: Task[Option[SessionToken]]): Task[A] =
    get[A](uri, Duration.Inf, Nil)

  /** HTTP Get with Accept: application/json. */
  final def get[A: Decoder](uri: Uri, timeout: Duration)(implicit s: Task[Option[SessionToken]]): Task[A] =
    get[A](uri, timeout, Nil)

  /** HTTP Get with Accept: application/json. */
  final def get[A: Decoder](uri: Uri, headers: List[HttpHeader])
    (implicit s: Task[Option[SessionToken]])
  : Task[A] =
    get[A](uri, Duration.Inf, headers)

  /** HTTP Get with Accept: application/json. */
  final def get[A: Decoder](uri: Uri, timeout: Duration, headers: List[HttpHeader])
    (implicit s: Task[Option[SessionToken]])
  : Task[A] =
    get_[A](uri, headers ::: Accept(`application/json`) :: Nil)

  final def get_[A: FromResponseUnmarshaller](uri: Uri, headers: List[HttpHeader] = Nil)
    (implicit s: Task[Option[SessionToken]])
  : Task[A] =
    sendReceive(HttpRequest(GET, AkkaUri(uri.string), headers ::: `Cache-Control`(`no-cache`, `no-store`) :: Nil))
      .flatMap(unmarshal[A](GET, uri))

  final def post[A: Encoder, B: Decoder](uri: Uri, data: A)(implicit s: Task[Option[SessionToken]]): Task[B] =
    post2[A, B](uri, data, Nil)

  final def postWithHeaders[A: Encoder, B: Decoder](uri: Uri, data: A, headers: List[HttpHeader])
    (implicit s: Task[Option[SessionToken]])
  : Task[B] =
    post2[A, B](uri, data, headers)

  private def post2[A: Encoder, B: Decoder](uri: Uri, data: A, headers: List[HttpHeader])
    (implicit s: Task[Option[SessionToken]])
  : Task[B] =
    post_[A](uri, data, headers ::: Accept(`application/json`) :: Nil)
      .flatMap(unmarshal[B](POST, uri))

  final def postDiscardResponse[A: Encoder](uri: Uri, data: A, allowedStatusCodes: Set[Int] = Set.empty)
    (implicit s: Task[Option[SessionToken]])
  : Task[Int] =
    post_[A](uri, data, Accept(`application/json`) :: Nil)
      .flatMap { httpResponse =>
        Task.defer {
          if (!httpResponse.status.isSuccess && !allowedStatusCodes(httpResponse.status.intValue)) {
            Task.deferFuture(httpResponse.entity.toStrict(FailureTimeout, maxBytes = HttpErrorContentSizeMaximum))
              .flatMap(entity =>
                Task.raiseError(throw new HttpException(POST, uri, httpResponse, entity.data.utf8String)))
          } else
            Task.pure(httpResponse.status.intValue)
        }.guarantee(Task {
          httpResponse.discardEntityBytes()
        })
      }

  final def post_[A: Encoder](uri: Uri, data: A, headers: List[HttpHeader])
    (implicit s: Task[Option[SessionToken]])
  : Task[HttpResponse] =
    for {
      entity <- Task.deferFuture(executeOn(materializer.executionContext)(implicit ec => Marshal(data).to[RequestEntity]))
      response <- sendReceive(
        HttpRequest(POST, uri.asAkka, headers, entity),
        logData = Some(data.toString))
    } yield response

  final def postRaw(uri: Uri, headers: List[HttpHeader], entity: RequestEntity)
    (implicit s: Task[Option[SessionToken]])
  : Task[HttpResponse] =
    sendReceive(HttpRequest(POST, uri.asAkka, headers, entity))

  final def sendReceive(request: HttpRequest, logData: => Option[String] = None)
    (implicit sessionTokenTask: Task[Option[SessionToken]])
  : Task[HttpResponse] =
    sessionTokenTask.flatMap(sessionToken =>
      Task.defer {
        withCheckedAgentUri(request) { request =>
          val number = counter.incrementAndGet()
          val headers = sessionToken.map(token => RawHeader(SessionToken.HeaderName, token.secret.string))
          val req = encodeGzip(request.withHeaders(headers ++: request.headers ++: standardHeaders))
          var since = now
          def responseLogPrefix = s"$toString: #$number ${requestToString(req, logData, isResponse = true)} ${since.elapsed.pretty}"
          loggingTimerResource(responseLogPrefix).use { _ =>
            @volatile var cancelled = false
            var responseFuture: Future[HttpResponse] = null
            Task.deferFutureAction { implicit s =>
              logger.trace(s"$toString: #$number ${requestToString(req, logData)}")
              since = now
              if (closed) throw new IllegalStateException(s"$toString has been closed")
              val httpsContext = if (request.uri.scheme == "https") httpsConnectionContext else http.defaultClientHttpsContext
              responseFuture = http.singleRequest(req, httpsContext)
              responseFuture.recover {
                case t if cancelled =>
                  logger.debug(s"$responseLogPrefix => Error after cancel: ${t.toStringWithCauses}")
                  // Fake response to avoid completing Future with a Failure, which is logged by thread's reportFailure
                  // TODO Akka's max-open-requests may be exceeded when new requests are opened
                  //  while many cancelled request are still not completed by Akka until some Akka (connection) timeout.
                  //  Anyway, the caller's code should be fault-tolerant.
                  HttpResponse(
                    StatusCodes.GatewayTimeout,
                    entity = HttpEntity.Strict(`text/plain(UTF-8)`, ByteString("Cancelled in AkkaHttpClient")))
              }
            } .guaranteeCase(exitCase => Task {
                logger.trace(s"$responseLogPrefix => $exitCase")
                if (exitCase != ExitCase.Canceled) {
                  responseFuture = null  // Release memory
                }
              })
              .doOnCancel(Task.defer {
                // TODO Cancelling does not cancel the ongoing Akka operation. Akka does not free the connection.
                cancelled = true
                responseFuture match {
                  case null => Task.unit
                  case r =>
                    responseFuture = null
                    discardResponse(responseLogPrefix, r)
                }
              })
              .materialize.map { tried =>
                tried match {
                  case Failure(t) =>
                    logger.debug(s"$responseLogPrefix => failed with ${t.toStringWithCauses}")
                  case Success(response) if response.status.isFailure =>
                    logger.debug(s"$responseLogPrefix => failed with HTTP ${response.status}")
                  case _ =>
                }
                tried
              }
              .dematerialize
              .map(decodeResponse)/*decompress*/
              .map(_.addHeader(InternalHeader(number)))
          }
        }
      })

  private def discardResponse(logPrefix: => String, responseFuture: Future[HttpResponse]): Task[Unit] =
    Task.fromFuture(responseFuture)
      .map[Unit] { response =>
        logger.debug(s"$logPrefix: discardEntityBytes()")
        response.discardEntityBytes()
      }
      .onErrorHandle(_ => ())  // Do not log lost exceptions

  private val emptyLoggingTimerResource = Resource.make(Task.pure(Cancelable.empty))(_ => Task.unit)

  private def loggingTimerResource[A](logPrefix: => String): Resource[Task, Cancelable] =
    if (logger.underlying.isDebugEnabled)
      Resource.make(
        Task.deferAction(scheduler => Task {
          var timer: Cancelable = null
          timer = scheduler.scheduleAtFixedRate(5.seconds, 10.seconds) {
            if (closed) {
              logger.debug(s"$logPrefix => Closed while waiting for response")
              if (timer != null) timer.cancel()
            } else
              logger.debug(s"$logPrefix => Still waiting for response")
          }
          timer
        })
      )(timer => Task { timer.cancel() })
    else
      emptyLoggingTimerResource

  private def unmarshal[A: FromResponseUnmarshaller](method: HttpMethod, uri: Uri)(httpResponse: HttpResponse): Task[A] =
    Task.deferFuture(
      executeOn(materializer.executionContext) { implicit ec =>
        if (httpResponse.status.isSuccess)
          Unmarshal(httpResponse).to[A]
            .recover { case t =>
              if (!materializer.isShutdown) {
                logger.debug(s"$toString: Error when unmarshalling response of ${method.name} $uri: ${t.toStringWithCauses}", t)
              }
              throw t
            }
        else
          httpResponse.entity.toStrict(FailureTimeout, maxBytes = HttpErrorContentSizeMaximum)
            .map(entity => throw new HttpException(method, uri, httpResponse, entity.data.utf8String))
      })

  private def withCheckedAgentUri[A](request: HttpRequest)(body: HttpRequest => Task[A]): Task[A] =
    toCheckedAgentUri(request.uri.asUri) match {
      case Right(uri) => body(request.copy(uri = uri.asAkka))
      case Left(problem) => Task.raiseError(problem.throwable)
    }

  private[http] final def toCheckedAgentUri(uri: Uri): Checked[Uri] =
    checkAgentUri(normalizeAgentUri(uri))

  private[http] final def normalizeAgentUri(uri: Uri): Uri = {
    val AkkaUri(scheme, authority, path, query, fragment) = uri.asAkka
    if (scheme.isEmpty && authority.isEmpty)
      AkkaUri(baseAkkaUri.scheme, baseAkkaUri.authority, path, query, fragment).asUri
    else
      uri
  }

  /** Checks `uri` againts `baseUri` - scheme and authority must be equal. */
  private[http] final def checkAgentUri(uri: Uri): Checked[Uri] = {
    val akkaUri = uri.asAkka
    if (akkaUri.scheme == baseAkkaUri.scheme &&
      akkaUri.authority == baseAkkaUri.authority &&
      akkaUri.path.toString.startsWith(uriPrefixPath))
      Right(uri)
    else
      Left(Problem(s"URI '$uri' does not match $baseUri$uriPrefixPath"))
  }

  private def requestToString(request: HttpRequest, logData: => Option[String], isResponse: Boolean = false): String = {
    val b = new StringBuilder(200)
    b.append(request.method.value.pipeIf(isResponse, _.toLowerCase(Locale.ROOT)))
    b.append(' ')
    b.append(request.uri)
    if (!request.entity.isKnownEmpty) {
      for (o <- logData) {
        b.append(' ')
        b.append(o)
      }
    }
    b.toString
  }

  override def isIgnorableStackTrace(throwable: Throwable) = throwable match {
    case _: akka.stream.StreamTcpException => false
    case _ => true
  }

  override def toString = s"$baseUri${if (name.isEmpty) "" else s" »$name«"}"
}

object AkkaHttpClient
{
  private val ErrorMessageLengthMaximum = 10000
  private val HttpErrorContentSizeMaximum = ErrorMessageLengthMaximum + 100
  private val FailureTimeout = 10.seconds
  private val logger = Logger(getClass)

  final class Standard(
    protected val baseUri: Uri,
    protected val uriPrefixPath: String,
    protected val actorSystem: ActorSystem,
    /** To provide a client certificate to server. */
    protected val keyStoreRef: Option[KeyStoreRef] = None,
    /** To trust the server's certificate. */
    protected val trustStoreRef: Option[TrustStoreRef] = None,
    protected val name: String = "")
  extends AkkaHttpClient

  def sessionMayBeLost(t: Throwable): Boolean =
    t match {
      case t: HttpException if t.status == Unauthorized || t.status == Forbidden => true
      case _ => false
    }

  final class HttpException private[http](method: HttpMethod, val uri: Uri, httpResponse: HttpResponse, val dataAsString: String)
  extends HttpClient.HttpException
  {
    def statusInt = status.intValue

    // Don't publish httpResponse because its entity stream has already been consumed for dataAsString
    def status: StatusCode = httpResponse.status

    def header[A >: Null <: HttpHeader: ClassTag]: Option[A] = httpResponse.header[A]

    override def toString = getMessage

    override def getMessage =
      s"$prefixString => ${problem getOrElse shortDataString}"

    private def prefixString = {
      val number = httpResponse.header[InternalHeader].fold("")(o => s" #${o.value}")
      s"HTTP ${httpResponse.status}:$number ${method.value} $uri"
    }

    private def shortDataString = dataAsString.truncateWithEllipsis(ErrorMessageLengthMaximum)

    lazy val problem: Option[Problem] =
      if (httpResponse.entity.contentType == ContentTypes.`application/json`)
        io.circe.parser.decode[Problem](dataAsString) match {
          case Left(error) =>
            logger.debug(s"$uri: $prefixString, Problem cannot be parsed: $error - $dataAsString")
            None
          case Right(o) => Some(o)
        }
      else
        None
  }

  private final case class InternalHeader(number: Long) extends CustomHeader {
    val name = "X-JS7-Internal-Header"
    val value = number.toString
    def renderInRequests = false
    def renderInResponses = false
  }
}
