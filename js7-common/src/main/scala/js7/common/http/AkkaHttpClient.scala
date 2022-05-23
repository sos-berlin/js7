package js7.common.http

import akka.actor.ActorSystem
import akka.http.scaladsl.marshalling.Marshal
import akka.http.scaladsl.model.HttpEntity.{Chunk, LastChunk}
import akka.http.scaladsl.model.HttpMethods.{GET, POST}
import akka.http.scaladsl.model.MediaTypes.`application/json`
import akka.http.scaladsl.model.StatusCodes.GatewayTimeout
import akka.http.scaladsl.model.headers.CacheDirectives.{`no-cache`, `no-store`}
import akka.http.scaladsl.model.headers.{Accept, ModeledCustomHeader, ModeledCustomHeaderCompanion, `Cache-Control`}
import akka.http.scaladsl.model.{ContentTypes, ErrorInfo, HttpEntity, HttpHeader, HttpMethod, HttpRequest, HttpResponse, RequestEntity, StatusCode, Uri => AkkaUri}
import akka.http.scaladsl.unmarshalling.{FromResponseUnmarshaller, Unmarshal}
import akka.http.scaladsl.{ConnectionContext, Http}
import akka.stream.Materializer
import akka.util.ByteString
import cats.effect.ExitCase
import io.circe.syntax._
import io.circe.{Decoder, Encoder, Json}
import java.util.Locale
import js7.base.auth.SessionToken
import js7.base.circeutils.CirceUtils.RichJson
import js7.base.circeutils.CirceUtils.implicits._
import js7.base.configutils.Configs.RichConfig
import js7.base.data.ByteArray
import js7.base.data.ByteSequence.ops._
import js7.base.exceptions.HasIsIgnorableStackTrace
import js7.base.generic.SecretString
import js7.base.io.https.Https.loadSSLContext
import js7.base.io.https.HttpsConfig
import js7.base.log.CorrelId.currentCorrelId
import js7.base.log.LogLevel.syntax.LevelLogger
import js7.base.log.LogLevel.{Debug, Trace}
import js7.base.log.{CorrelId, Logger}
import js7.base.monixutils.MonixBase.syntax._
import js7.base.problem.Checked._
import js7.base.problem.{Checked, Problem}
import js7.base.time.ScalaTime._
import js7.base.time.Stopwatch.bytesPerSecondString
import js7.base.utils.ByteSequenceToLinesObservable
import js7.base.utils.MonixAntiBlocking.executeOn
import js7.base.utils.ScalaUtils.syntax._
import js7.base.web.{HttpClient, Uri}
import js7.common.akkahttp.ByteSequenceChunkerObservable.syntax._
import js7.common.akkahttp.CirceJsonSupport.{jsonMarshaller, jsonUnmarshaller}
import js7.common.akkautils.ByteStrings.syntax._
import js7.common.http.AkkaHttpClient._
import js7.common.http.AkkaHttpUtils.{RichAkkaAsUri, RichAkkaUri, decompressResponse, encodeGzip}
import js7.common.http.JsonStreamingSupport.{StreamingJsonHeader, StreamingJsonHeaders, `application/x-ndjson`}
import js7.common.http.StreamingSupport._
import monix.eval.Task
import monix.execution.atomic.AtomicLong
import monix.reactive.Observable
import org.jetbrains.annotations.TestOnly
import scala.concurrent.Future
import scala.concurrent.duration.Deadline.now
import scala.concurrent.duration._
import scala.reflect.ClassTag
import scala.util.Success
import scala.util.control.NoStackTrace

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
  private lazy val jsonReadAhead = http.system.settings.config.getInt("js7.web.client.json-read-ahead")
  private lazy val chunkSize = http.system.settings.config.memorySizeAsInt("js7.web.chunk-size").orThrow
  @volatile private var closed = false

  protected def httpsConfig: HttpsConfig

  private lazy val httpsConnectionContext =
    if (httpsConfig.keyStoreRef.isEmpty && httpsConfig.trustStoreRefs.isEmpty)
      http.defaultClientHttpsContext
    else
      ConnectionContext.httpsClient(
        loadSSLContext(httpsConfig.keyStoreRef, httpsConfig.trustStoreRefs))

  final def materializer: Materializer = implicitly[Materializer]

  def close() = {
    logger.trace(s"$toString: close")
    closed = true
  }

  final def getDecodedLinesObservable[A: Decoder](uri: Uri, responsive: Boolean = false)
    (implicit s: Task[Option[SessionToken]])
  : Task[Observable[A]] =
    getRawLinesObservable(uri)
      .map(_
        .mapParallelBatch(
          batchSize = jsonReadAhead / sys.runtime.availableProcessors,
          responsive = responsive)(
          _.parseJsonAs[A].orThrow))

  final def getRawLinesObservable(uri: Uri)(implicit s: Task[Option[SessionToken]])
  : Task[Observable[ByteArray]] =
    get_[HttpResponse](uri, StreamingJsonHeaders)
      .map(_.entity.withoutSizeLimit.dataBytes.toObservable)
      .pipeIf(logger.underlying.isDebugEnabled)(_.logTiming(_.size, (d, s, _) =>
        if (d >= 1.s && s > 10_000_000)
          logger.debug(s"$toString: get $uri: ${bytesPerSecondString(d, s)}")))
      .map(_
        .flatMap(new ByteSequenceToLinesObservable))
      .map(_.onErrorRecoverWith(ignoreIdleTimeout orElse endStreamOnNoMoreElementNeeded))
      .onErrorRecover(ignoreIdleTimeout)

  private def endStreamOnNoMoreElementNeeded: PartialFunction[Throwable, Observable[Nothing]] = {
    case t @ akka.stream.SubscriptionWithCancelException.NoMoreElementsNeeded =>
      // On NoMoreElementsNeeded the Observable ends silently !!! Maybe harmless?
      logger.warn(s"Ignore ${t.toString}")
      if (t.getStackTrace.nonEmpty) logger.debug(s"Ignore $t", t)
      Observable.empty
  }

  private def ignoreIdleTimeout: PartialFunction[Throwable, Observable[Nothing]] = {
    case t: akka.stream.scaladsl.TcpIdleTimeoutException =>
      // Idle timeout is silently ignored !!! Maybe harmless?
      logger.warn(s"Ignore ${t.toString}")
      if (t.getStackTrace.nonEmpty) logger.debug(s"Ignore $t", t)
      Observable.empty
  }

  /** HTTP Get with Accept: application/json. */
  final def get[A: Decoder](uri: Uri)(implicit s: Task[Option[SessionToken]]): Task[A] =
    get[A](uri, Nil)

  /** HTTP Get with Accept: application/json. */
  final def get[A: Decoder](uri: Uri, headers: List[HttpHeader])
    (implicit s: Task[Option[SessionToken]])
  : Task[A] =
    get_[A](uri, AcceptJson ::: headers)

  final def get_[A: FromResponseUnmarshaller](uri: Uri, headers: List[HttpHeader] = Nil)
    (implicit s: Task[Option[SessionToken]])
  : Task[A] =
    sendReceive(HttpRequest(GET, AkkaUri(uri.string), `Cache-Control`(`no-cache`, `no-store`) :: headers))
      .flatMap(unmarshal[A](GET, uri))

  final def post[A: Encoder, B: Decoder](uri: Uri, data: A)(implicit s: Task[Option[SessionToken]]): Task[B] =
    post2[A, B](uri, data, Nil)

  final def postObservable[A: Encoder, B: Decoder](uri: Uri, data: Observable[A])
    (implicit s: Task[Option[SessionToken]])
  : Task[B] =
    data
      .mapParallelBatch()(_
        .asJson.toByteSequence[ByteString]
        .concat(LF)
        .chunk(chunkSize))
      .flatMap(Observable.fromIterable)
      .map(Chunk(_))
      .append(LastChunk)
      .toAkkaSourceTask
      .flatMap(akkaChunks =>
        sendReceive(
          HttpRequest(POST, uri.asAkka, Accept(`application/json`) :: Nil,
            HttpEntity.Chunked(`application/x-ndjson`.toContentType, akkaChunks)),
          logData = Some("postObservable")))
      .flatMap(unmarshal[B](POST, uri))

  @TestOnly
  final def postObservableJsonString(uri: Uri, data: Observable[String])(implicit s: Task[Option[SessionToken]]): Task[Json] =
    data
      .map(o => ByteString(o) ++ LF)
      .chunk(chunkSize)
      .map(Chunk(_))
      .append(LastChunk)
      .toAkkaSourceTask
      .flatMap(akkaChunks =>
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
    post_[A](uri, data, AcceptJson ::: headers)
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
                Task.raiseError(new HttpException(POST, uri, httpResponse, entity.data.utf8String)))
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
      // Maybe executeOn avoid blocking with a single thread Scheduler,
      // but sometimes throws RejectedExecutionException in test build
      //entity <- Task.deferFuture(executeOn(materializer.executionContext)(implicit ec => Marshal(data).to[RequestEntity]))
      entity <- Task.deferFutureAction(implicit s => Marshal(data).to[RequestEntity])
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
        if (closed) {
          logger.debug(s"(WARN) AkkaHttpClient has actually been closed: ${requestToString(request, logData)}")
        }
        val number = requestCounter.incrementAndGet()
        val headers = sessionToken.map(token => `x-js7-session`(token)).toList :::
          `x-js7-request-id`(s"#$number") ::
          currentCorrelId.toOption.map(`x-js7-correlation-id`(_)).toList :::
          request.headers.toList :::
          standardHeaders
        val req = request.withHeaders(headers).pipeIf(useCompression)(encodeGzip)
        @volatile var canceled = false
        var responseFuture: Future[HttpResponse] = null
        val since = now
        lazy val logPrefix = s"$toString: ${sessionToken.fold("")(_.short + " ")}#$number"
        lazy val responseLog0 = s"$logPrefix ${requestToString(req, logData, isResponse = true)} "
        def responseLogPrefix = responseLog0 + since.elapsed.pretty
        logger.trace(s">--> $logPrefix: ${requestToString(req, logData)}")
        Task
          .deferFutureAction { scheduler =>
            responseFuture = http.singleRequest(req,
              if (req.uri.scheme == "https") httpsConnectionContext else http.defaultClientHttpsContext)
            responseFuture
              .recover { case t if canceled =>
                logger.trace(s"$logPrefix: Ignored after cancel: ${t.toStringWithCauses}")
                // Task guarantee below may report a failure after cancel
                // via thread pools's reportFailure. To avoid this, we convert the failure
                // to a dummy successful response, which will get lost immediately.
                HttpResponse(GatewayTimeout, entity = "CANCELED")
              }(scheduler)
          }
          .guaranteeCase {
            case ExitCase.Canceled => Task {
              canceled = true
              logger.debug(s"$responseLogPrefix => canceled")
              if (responseFuture != null) {
                // TODO Akka's max-open-requests may be exceeded when new requests are opened
                //  while many canceled requests are still not completed by Akka
                //  until the server has reponded or some Akka (idle connection) timeout.
                //  Anyway, the caller's code should be fault-tolerant.
                // TODO Maybe manage own connection pool? Or switch to http4s?
                executeOn(materializer.executionContext) { implicit ec =>
                  responseFuture
                    .map(_
                      .discardEntityBytes().future.andThen { case tried =>
                        logger.debug(s"$responseLogPrefix discardResponse => " +
                          tried.fold(_.toStringWithCauses, _ => "ok"))
                      })
                    .recover { case _ => () }
                }
              }
            }

            case ExitCase.Error(throwable) => Task.defer {
              logger.debug(s"<--<💥 $responseLogPrefix => failed with ${throwable.toStringWithCauses}")
              Task.raiseError(toPrettyProblem(throwable).throwable)
            }

            case ExitCase.Completed => Task.unit
          }
          .map(decompressResponse)
          .pipeIf(logger.underlying.isDebugEnabled)(
            _.whenItTakesLonger() {
              val level = if (request.headers contains StreamingJsonHeader) Trace else Debug
              _ => Task(logger.underlying.log(level,
                "⏳ " + responseLogPrefix + " => Still waiting for response" +
                  (closed ?? " (closed)")))
            }
            .tapEval(response => Task {
              logger.debug(
                s"<--<${response.status.isFailure ?? "❓"} $responseLogPrefix => ${response.status}")
            }))
      })

  private def unmarshal[A: FromResponseUnmarshaller](method: HttpMethod, uri: Uri)(httpResponse: HttpResponse) =
    Task.deferFuture[A](
      executeOn(materializer.executionContext) { implicit ec =>
        if (httpResponse.status.isSuccess)
          Unmarshal(httpResponse).to[A]
            .recover { case t =>
              if (!materializer.isShutdown) {
                logger.debug(
                  s"$toString: Error when unmarshalling response of ${method.name} $uri: ${t.toStringWithCauses}", t)
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

  private def requestToString(request: HttpRequest, logData: => Option[String], isResponse: Boolean = false) = {
    val b = new StringBuilder(300)
    b.append(request.method.value.pipeIf(isResponse)(_.toLowerCase(Locale.ROOT)))
    b.append(' ')
    b.append(request.uri)
    if (!request.entity.isKnownEmpty) {
      for (o <- logData) {
        b.append(' ')
        b.append(o.truncateWithEllipsis(200, showLength = true, firstLineOnly = true))
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
  final case class `x-js7-session`(sessionToken: SessionToken)
  extends ModeledCustomHeader[`x-js7-session`] {
    val companion = `x-js7-session`
    def value = sessionToken.secret.string
    val renderInRequests = true
    val renderInResponses = false
  }
  object `x-js7-session` extends ModeledCustomHeaderCompanion[`x-js7-session`] {
    val name = "x-js7-session"
    def parse(value: String) = Success(new `x-js7-session`(SessionToken(SecretString(value))))
  }

  final case class `x-js7-request-id`(value: String)
  extends ModeledCustomHeader[`x-js7-request-id`] {
    val companion = `x-js7-request-id`
    val renderInRequests = true
    val renderInResponses = false
  }
  object `x-js7-request-id` extends ModeledCustomHeaderCompanion[`x-js7-request-id`] {
    val name = "x-js7-request-id"
    def parse(value: String) = Success(new `x-js7-request-id`(value))
  }

  final case class `x-js7-correlation-id`(correlId: CorrelId)
  extends ModeledCustomHeader[`x-js7-correlation-id`] {
    val companion = `x-js7-correlation-id`
    // Convert to ASCII
    def value = correlId.toAscii
    val renderInRequests = true
    val renderInResponses = false
  }
  object `x-js7-correlation-id` extends ModeledCustomHeaderCompanion[`x-js7-correlation-id`] {
    val name = "x-js7-correlation-id"
    def parse(asciiString: String) =
      CorrelId.checked(asciiString).map(`x-js7-correlation-id`(_)).asTry
  }

  private val ErrorMessageLengthMaximum = 10000
  private val HttpErrorContentSizeMaximum = ErrorMessageLengthMaximum + 100
  private val FailureTimeout = 10.seconds
  private val logger = Logger(getClass)
  private val LF = ByteString("\n")
  private val AcceptJson = Accept(`application/json`) :: Nil
  private val requestCounter = AtomicLong(0)

  final class Standard(
    protected val baseUri: Uri,
    protected val uriPrefixPath: String = "",
    protected val actorSystem: ActorSystem,
    protected val httpsConfig: HttpsConfig = HttpsConfig.empty,
    protected val name: String = "")
  extends AkkaHttpClient

  private val connectionWasClosedUnexpectedly = Problem.pure("Connection was closed unexpectedly")

  private val AkkaTcpCommandRegex = """Tcp command \[([A-Za-z]+)\(([^,)]+).*""".r

  def toPrettyProblem(problem: Problem): Problem =
    problem.throwableOption.fold(problem) { throwable =>
      val pretty = toPrettyProblem(throwable)
      pretty.throwableOption match {
        case Some(t) if t eq throwable =>
          problem
        case _ =>
          logger.debug(s"toPrettyProblem($problem) => $pretty")
          pretty
      }
    }

  def toPrettyProblem(throwable: Throwable): Problem = {
    def default = Problem.fromThrowable(throwable)
    throwable match {
      case akka.http.scaladsl.model.EntityStreamException(ErrorInfo(summary, _)) =>
        if (summary contains "connection was closed unexpectedly")
          connectionWasClosedUnexpectedly
        else Problem.pure(summary)

      case t if t.getClass.getName endsWith "UnexpectedConnectionClosureException" =>
        connectionWasClosedUnexpectedly

      case t: akka.stream.StreamTcpException =>
        t.getMessage match {
          case AkkaTcpCommandRegex(command, host_) =>
            val host = host_.replace("/<unresolved>", "")
            val prefix = s"TCP $command $host"
            t.getCause match {
              case t: java.net.SocketException =>
                Problem(prefix + ": " + t.getMessage)
              case t: java.net.UnknownHostException =>
                Problem(prefix + ": " + t.toString stripPrefix "java.net.")
              case _ => default
            }
          case _ => default
        }

      case _ => default
    }
  }

  final class HttpException private[http](
    method: HttpMethod,
    val uri: Uri,
    httpResponse: HttpResponse,
    val dataAsString: String)
  extends HttpClient.HttpException with NoStackTrace
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
