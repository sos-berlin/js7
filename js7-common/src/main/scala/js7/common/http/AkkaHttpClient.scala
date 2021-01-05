package js7.common.http

import akka.actor.ActorSystem
import akka.http.scaladsl.marshalling.Marshal
import akka.http.scaladsl.model.HttpEntity.{ChunkStreamPart, LastChunk}
import akka.http.scaladsl.model.HttpMethods.{GET, POST}
import akka.http.scaladsl.model.MediaTypes.`application/json`
import akka.http.scaladsl.model.StatusCodes.GatewayTimeout
import akka.http.scaladsl.model.headers.CacheDirectives.{`no-cache`, `no-store`}
import akka.http.scaladsl.model.headers.{Accept, RawHeader, `Cache-Control`}
import akka.http.scaladsl.model.{ContentTypes, ErrorInfo, HttpEntity, HttpHeader, HttpMethod, HttpRequest, HttpResponse, RequestEntity, StatusCode, Uri => AkkaUri}
import akka.http.scaladsl.unmarshalling.{FromResponseUnmarshaller, Unmarshal}
import akka.http.scaladsl.{ConnectionContext, Http}
import akka.stream.Materializer
import akka.util.ByteString
import cats.effect.ExitCase
import io.circe.{Decoder, Encoder, Json}
import java.util.Locale
import js7.base.auth.SessionToken
import js7.base.circeutils.CirceUtils.implicits._
import js7.base.data.ByteArray
import js7.base.exceptions.HasIsIgnorableStackTrace
import js7.base.monixutils.MonixBase.syntax._
import js7.base.problem.Checked._
import js7.base.problem.{Checked, Problem}
import js7.base.time.ScalaTime._
import js7.base.time.Stopwatch.bytesPerSecondString
import js7.base.utils.ByteArrayToLinesObservable
import js7.base.utils.MonixAntiBlocking.executeOn
import js7.base.utils.ScalaUtils.syntax._
import js7.base.web.{HttpClient, Uri}
import js7.common.akkahttp.https.Https.loadSSLContext
import js7.common.akkahttp.https.{KeyStoreRef, TrustStoreRef}
import js7.common.akkautils.ByteStrings.syntax._
import js7.common.akkautils.JsonObservableForAkka.syntax._
import js7.common.http.AkkaHttpClient._
import js7.common.http.AkkaHttpUtils.{RichAkkaAsUri, RichAkkaUri, decompressResponse, encodeGzip}
import js7.common.http.CirceJsonSupport._
import js7.common.http.JsonStreamingSupport.{StreamingJsonHeaders, `application/x-ndjson`}
import js7.common.http.StreamingSupport._
import js7.common.scalautil.Logger
import monix.eval.Task
import monix.execution.atomic.AtomicLong
import monix.reactive.Observable
import org.jetbrains.annotations.TestOnly
import scala.concurrent.duration.Deadline.now
import scala.concurrent.duration._
import scala.concurrent.{Future, Promise}
import scala.reflect.ClassTag
import scala.reflect.runtime.universe._

/**
  * @author Joacim Zschimmer
  */
trait AkkaHttpClient extends AutoCloseable with HttpClient with HasIsIgnorableStackTrace
{
  implicit protected def actorSystem: ActorSystem
  protected def baseUri: Uri
  protected def uriPrefixPath: String
  protected def name: String
  protected def standardHeaders: List[HttpHeader] = Nil

  private lazy val http = Http(actorSystem)
  private lazy val baseAkkaUri = AkkaUri(baseUri.string)
  private lazy val useCompression = http.system.settings.config.getBoolean("js7.web.client.compression")
  @volatile private var closed = false

  protected def keyStoreRef: Option[KeyStoreRef]
  protected def trustStoreRefs: Seq[TrustStoreRef]

  private lazy val httpsConnectionContext = {
    logger.debug(s"keyStoreRef=$keyStoreRef trustStoreRefs=$trustStoreRefs")
    if (keyStoreRef.isEmpty && trustStoreRefs.isEmpty)
      http.defaultClientHttpsContext
    else
      ConnectionContext.httpsClient(loadSSLContext(keyStoreRef, trustStoreRefs))
  }

  final def materializer: Materializer = implicitly[Materializer]

  def close() = {
    logger.trace(s"$toString: close")
    closed = true
  }

  def isClosed = closed

  final def getDecodedLinesObservable[A: Decoder](uri: Uri)(implicit s: Task[Option[SessionToken]]) =
    getRawLinesObservable(uri)
      .map(_.map(_.parseJsonAs[A].orThrow))

  final def getDecodedLinesObservableBatch[A: Decoder](uri: Uri)(implicit s: Task[Option[SessionToken]])
  : Task[Observable[A]] =
    getRawLinesObservable(uri)
      .map(_
        .mapParallelOrderedBatch()(_
          .parseJsonAs[A].orThrow))

  final def getRawLinesObservable(uri: Uri)(implicit s: Task[Option[SessionToken]])
  : Task[Observable[ByteArray]] =
    get_[HttpResponse](uri, StreamingJsonHeaders)
      .map(_.entity.withoutSizeLimit.dataBytes.toObservable)
      .pipeIf(logger.underlying.isDebugEnabled)(_.logTiming(_.size, (d, s, _) =>
        if (d >= 1.s && s > 10_000_000)
          logger.debug(s"$toString: get $uri: ${bytesPerSecondString(d, s)}")))
      .map(_
        .map(_.toByteArray)
        .flatMap(new ByteArrayToLinesObservable))
      .onErrorRecover {
        case akka.stream.SubscriptionWithCancelException.NoMoreElementsNeeded =>
          logger.debug("Return NoMoreElementsNeeded exception as empty Observable")
          Observable.empty
      }

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

  final def postObservable[A: Encoder: TypeTag, B: Decoder](uri: Uri, data: Observable[A])
    (implicit s: Task[Option[SessionToken]])
  : Task[B] =
    Task.deferAction(implicit s => Task(data
      .encodeJson
      .map(o => ChunkStreamPart(o ++ LF))
      .append(LastChunk)
      .toAkkaSource)
    ) .flatMap(akkaChunks =>
        sendReceive(
          HttpRequest(POST, uri.asAkka, Accept(`application/json`) :: Nil,
            HttpEntity.Chunked(`application/x-ndjson`.toContentType, akkaChunks)),
          logData = Some("postObservable")))
      .flatMap(unmarshal[B](POST, uri))

  @TestOnly
  final def postObservableJsonString(uri: Uri, data: Observable[String])(implicit s: Task[Option[SessionToken]]): Task[Json] =
    Task.deferAction(implicit s => Task(data
      .map(o => ChunkStreamPart(ByteString(o) ++ LF))
      .append(LastChunk)
      .toAkkaSource)
    ) .flatMap(akkaChunks =>
        sendReceive(
          HttpRequest(POST, uri.asAkka, Accept(`application/json`) :: Nil,
            HttpEntity.Chunked(`application/x-ndjson`.toContentType, akkaChunks)),
          logData = Some("postObservable")))
      .flatMap(unmarshal[Json](POST, uri))

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
    withCheckedAgentUri(request)(request =>
      sessionTokenTask.flatMap { sessionToken =>
        if (closed) logger.debug(s"(WARN) AkkaHttpClient has actually been closed: ${requestToString(request, logData)}")
        val number = requestCounter.incrementAndGet()
        val req = request.withHeaders(
          sessionToken.map(token => RawHeader(SessionToken.HeaderName, token.secret.string)) ++: request.headers ++: standardHeaders
        ).pipeIf(useCompression)(encodeGzip)
        @volatile var canceled = false
        val promise = Promise[HttpResponse]()
        var responseFuture: Future[HttpResponse] = null
        val since = now
        def responseLogPrefix = s"$toString: #$number ${sessionToken.fold("-")(_.short)} " +
          s"${requestToString(req, logData, isResponse = true)} ${since.elapsed.pretty}"
        Task
          .deferFuture {
            logger.trace(s"$toString: #$number ${requestToString(req, logData)}")
            responseFuture = http.singleRequest(
              req,
              if (req.uri.scheme == "https") httpsConnectionContext else http.defaultClientHttpsContext)
            promise.completeWith(responseFuture)
            promise.future
          }
          .guaranteeCase {
            case ExitCase.Canceled if responseFuture != null =>
              logger.debug(s"$responseLogPrefix => canceled")
              canceled = true
              // Complete the future to complete Task.deferFuture above (???)
              promise.trySuccess(HttpResponse(GatewayTimeout, entity = "Canceled in AkkaHttpClient"))
              // TODO Akka's max-open-requests may be exceeded when new requests are opened
              //  while many canceled request are still not completed by Akka
              //  until some Akka (idle connection) timeout.
              //  Anyway, the caller's code should be fault-tolerant.
              discardResponse(responseLogPrefix, responseFuture)
            case ExitCase.Error(t) =>
              Task(logger.debug(s"$responseLogPrefix => failed with ${t.toStringWithCauses}"))
            case _ => Task.unit
          }
          .whenItTakesLonger()(_ => Task {
            logger.debug(s"$responseLogPrefix => Still waiting for response" +
              (canceled ?? " (canceled)") + (closed ?? " (closed)"))
          })
          .tapEval(response => Task {
            if (response.status.isFailure)
              logger.debug(s"$responseLogPrefix => failed with HTTP ${response.status}")
            else
              logger.trace(s"$responseLogPrefix => ${response.status}")
          })
          .map(decompressResponse)
      })

  private def discardResponse(logPrefix: => String, responseFuture: Future[HttpResponse]): Task[Unit] =
    Task.fromFuture(responseFuture)
      .map[Unit] { response =>
        logger.debug(s"$logPrefix: discardEntityBytes()")
        response.discardEntityBytes()
      }
      .onErrorHandle(_ => ())  // Do not log lost exceptions

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
            .flatMap(entity => Future.failed(
              new HttpException(method, uri, httpResponse, entity.data.utf8String)))
      })

  private def withCheckedAgentUri[A](request: HttpRequest)(body: HttpRequest => Task[A]): Task[A] =
    toCheckedAgentUri(request.uri.asUri) match {
      case Left(problem) => Task.raiseError(problem.throwable)
      case Right(uri) => body(request.withUri(uri.asAkka))
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
    val b = new StringBuilder(300)
    b.append(request.method.value.pipeIf(isResponse)(_.toLowerCase(Locale.ROOT)))
    b.append(' ')
    b.append(request.uri)
    if (!request.entity.isKnownEmpty) {
      for (o <- logData) {
        b.append(' ')
        b.append(o.truncateWithEllipsis(200, showLength = true))
      }
    }
    b.toString
  }

  override def isIgnorableStackTrace(throwable: Throwable) = throwable match {
    case _: akka.stream.StreamTcpException => false
    case _ => true
  }

  override def toString = s"$baseUri${name.nonEmpty ?? s" »$name«"}"
}

object AkkaHttpClient
{
  private val ErrorMessageLengthMaximum = 10000
  private val HttpErrorContentSizeMaximum = ErrorMessageLengthMaximum + 100
  private val FailureTimeout = 10.seconds
  private val logger = Logger(getClass)
  private val LF = ByteString("\n")
  private val requestCounter = AtomicLong(0)

  final class Standard(
    protected val baseUri: Uri,
    protected val uriPrefixPath: String,
    protected val actorSystem: ActorSystem,
    /** To provide a client certificate to server. */
    protected val keyStoreRef: Option[KeyStoreRef] = None,
    /** To trust the server's certificate. */
    protected val trustStoreRefs: Seq[TrustStoreRef] = Nil,
    protected val name: String = "")
  extends AkkaHttpClient

  private val connectionWasClosedUnexpectedly = Problem.pure("Connection was closed unexpectedly")

  def toPrettyProblem(problem: Problem): Problem = {
    val pretty = problem match {
      case Problem.IsThrowable(akka.http.scaladsl.model.EntityStreamException(ErrorInfo(summary, _))) =>
        if (summary contains "connection was closed unexpectedly") connectionWasClosedUnexpectedly
        else Problem.pure(summary)

      case Problem.IsThrowable(t)
        if t.getClass.getName endsWith "UnexpectedConnectionClosureException" =>
        connectionWasClosedUnexpectedly

      case Problem.IsThrowable(t: akka.stream.StreamTcpException) =>
        t.getCause match {
          case t: java.net.SocketException => Problem(t.getMessage)
          case t: java.net.UnknownHostException => Problem(t.toString stripPrefix "java.net.")
          case _ => problem
        }

      case _ => problem
    }
    if (pretty ne problem) logger.debug(s"toPrettyProblem($problem) => $pretty")
    pretty
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

    private def prefixString =
      s"HTTP ${httpResponse.status}: ${method.value} $uri"

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
}
