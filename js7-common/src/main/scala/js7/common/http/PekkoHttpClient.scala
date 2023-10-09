package js7.common.http

import cats.effect.concurrent.Deferred
import cats.effect.{ExitCase, Resource}
import cats.syntax.flatMap.*
import io.circe.syntax.*
import io.circe.{Decoder, Encoder, Json}
import java.util.Locale
import js7.base.auth.SessionToken
import js7.base.circeutils.CirceUtils.implicits.*
import js7.base.circeutils.CirceUtils.{RichCirceString, RichJson}
import js7.base.configutils.Configs.RichConfig
import js7.base.data.ByteArray
import js7.base.data.ByteSequence.ops.*
import js7.base.exceptions.HasIsIgnorableStackTrace
import js7.base.generic.SecretString
import js7.base.io.https.Https.loadSSLContext
import js7.base.io.https.HttpsConfig
import js7.base.log.{CorrelId, Logger}
import js7.base.monixutils.MonixBase.syntax.*
import js7.base.problem.Checked.*
import js7.base.problem.Problems.InvalidSessionTokenProblem
import js7.base.problem.{Checked, Problem}
import js7.base.time.ScalaTime.*
import js7.base.time.Stopwatch.bytesPerSecondString
import js7.base.utils.ByteSequenceToLinesObservable
import js7.base.utils.MonixAntiBlocking.executeOn
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.web.{HttpClient, Uri}
import js7.common.http.JsonStreamingSupport.{StreamingJsonHeader, StreamingJsonHeaders, `application/x-ndjson`}
import js7.common.http.PekkoHttpClient.*
import js7.common.http.PekkoHttpUtils.{RichPekkoAsUri, RichPekkoUri, RichResponseEntity, decompressResponse, encodeGzip}
import js7.common.http.StreamingSupport.*
import js7.common.pekkohttp.ByteSequenceChunkerObservable.syntax.*
import js7.common.pekkohttp.CirceJsonSupport.{jsonMarshaller, jsonUnmarshaller}
import js7.common.pekkoutils.ByteStrings.syntax.*
import monix.eval.Task
import monix.execution.atomic.AtomicLong
import monix.reactive.Observable
import org.apache.pekko
import org.apache.pekko.Done
import org.apache.pekko.actor.ActorSystem
import org.apache.pekko.http.scaladsl.marshalling.Marshal
import org.apache.pekko.http.scaladsl.model.ContentTypes.`application/json`
import org.apache.pekko.http.scaladsl.model.HttpCharsets.`UTF-8`
import org.apache.pekko.http.scaladsl.model.HttpEntity.{Chunk, ChunkStreamPart, Chunked, LastChunk}
import org.apache.pekko.http.scaladsl.model.HttpMethods.{GET, POST}
import org.apache.pekko.http.scaladsl.model.MediaTypes.`text/plain`
import org.apache.pekko.http.scaladsl.model.StatusCodes.{Forbidden, GatewayTimeout, Unauthorized}
import org.apache.pekko.http.scaladsl.model.headers.CacheDirectives.{`no-cache`, `no-store`}
import org.apache.pekko.http.scaladsl.model.headers.{Accept, ModeledCustomHeader, ModeledCustomHeaderCompanion, `Cache-Control`}
import org.apache.pekko.http.scaladsl.model.{ContentType, ErrorInfo, HttpEntity, HttpHeader, HttpMethod, HttpRequest, HttpResponse, MediaTypes, RequestEntity, StatusCode, Uri as PekkoUri}
import org.apache.pekko.http.scaladsl.unmarshalling.{FromResponseUnmarshaller, Unmarshal}
import org.apache.pekko.http.scaladsl.{ConnectionContext, Http}
import org.apache.pekko.stream.Materializer
import org.apache.pekko.util.ByteString
import org.jetbrains.annotations.TestOnly
import scala.concurrent.Future
import scala.concurrent.duration.Deadline.now
import scala.reflect.ClassTag
import scala.util.Success
import scala.util.control.{NoStackTrace, NonFatal}
import scala.util.matching.Regex

/**
  * @author Joacim Zschimmer
  */
trait PekkoHttpClient extends AutoCloseable, HttpClient, HasIsIgnorableStackTrace:
  
  implicit protected def actorSystem: ActorSystem
  protected def baseUri: Uri
  protected def uriPrefixPath: String
  protected def name: String
  protected def standardHeaders: List[HttpHeader] = Nil

  private lazy val http = Http(actorSystem)
  private lazy val basePekkoUri = PekkoUri(baseUri.string)
  private lazy val useCompression = http.system.settings.config.getBoolean("js7.web.client.compression")
  private lazy val jsonReadAhead = http.system.settings.config.getInt("js7.web.client.json-read-ahead")
  private lazy val chunkSize = http.system.settings.config.memorySizeAsInt("js7.web.chunk-size").orThrow
  @volatile private var closed = false

  protected def httpsConfig: HttpsConfig

  private lazy val httpsConnectionContext =
    if httpsConfig.keyStoreRef.isEmpty && httpsConfig.trustStoreRefs.isEmpty then
      http.defaultClientHttpsContext
    else
      ConnectionContext.httpsClient(
        loadSSLContext(httpsConfig.keyStoreRef, httpsConfig.trustStoreRefs))

  final def materializer: Materializer = implicitly[Materializer]

  def close() =
    logger.trace(s"$toString: close")
    closed = true

  final def getDecodedLinesObservable[A: Decoder](uri: Uri, responsive: Boolean = false)
    (implicit s: Task[Option[SessionToken]])
  : Task[Observable[A]] =
    getRawLinesObservable(uri)
      .map(_
        // Ignore empty keep-alives
        .collect { case o if o != EmptyLine => o }
        .mapParallelBatch(
          batchSize = jsonReadAhead / sys.runtime.availableProcessors,
          responsive = responsive)(
          _.parseJsonAs[A].orThrow))

  final def getRawLinesObservable(uri: Uri)(implicit s: Task[Option[SessionToken]])
  : Task[Observable[ByteArray]] =
    get_[HttpResponse](uri, StreamingJsonHeaders)
      .map(_.entity.withoutSizeLimit.dataBytes.toObservable)
      .pipeIf(logger.underlying.isDebugEnabled)(_.logTiming(_.size, (d, s, _) =>
        if d >= 1.s && s > 10_000_000 then
          logger.debug(s"get $uri: ${bytesPerSecondString(d, s)}")))
      .map(_
        .flatMap(new ByteSequenceToLinesObservable))
      .map(_.onErrorRecoverWith(ignoreIdleTimeout orElse endStreamOnNoMoreElementNeeded))
      .onErrorRecover(ignoreIdleTimeout)

  private def endStreamOnNoMoreElementNeeded: PartialFunction[Throwable, Observable[Nothing]] =
    case t @ pekko.stream.SubscriptionWithCancelException.NoMoreElementsNeeded =>
      // On NoMoreElementsNeeded the Observable ends silently !!! Maybe harmless?
      logger.warn(s"Ignore ${t.toString}")
      if hasRelevantStackTrace(t) then logger.debug(s"Ignore $t", t)
      Observable.empty

  private def ignoreIdleTimeout: PartialFunction[Throwable, Observable[Nothing]] =
    case t: pekko.stream.scaladsl.TcpIdleTimeoutException =>
      // Idle timeout is silently ignored !!! Maybe harmless?
      logger.warn(s"Ignore ${t.toString}")
      if hasRelevantStackTrace(t) then logger.debug(s"Ignore $t", t)
      Observable.empty

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
    sendReceive(HttpRequest(GET, PekkoUri(uri.string), `Cache-Control`(`no-cache`, `no-store`) :: headers))
      .flatMap(unmarshal[A](GET, uri))

  final def post[A: Encoder, B: Decoder](uri: Uri, data: A)(implicit s: Task[Option[SessionToken]]): Task[B] =
    post2[A, B](uri, data, Nil)

  final def postObservable[A: Encoder, B: Decoder](
    uri: Uri,
    data: Observable[A],
    responsive: Boolean = false,
    terminateStreamOnCancel: Boolean = false)
    (implicit s: Task[Option[SessionToken]])
  : Task[B] =
    def toNdJson(a: A) = a.asJson.toByteSequence[ByteString] ++ LF

    val chunks: Observable[ByteString] =
      if responsive then
        for a <- data yield toNdJson(a)
      else
        data
          .mapParallelBatch(responsive = responsive)(a =>
            toNdJson(a).chunk(chunkSize))
          .flatMap(Observable.fromIterable)

    Task.defer:
      val stop = Deferred.unsafe[Task, Unit]
      val stopped = Deferred.unsafe[Task, Unit]
      chunks
        .map(Chunk(_))
        .append[ChunkStreamPart](LastChunk)
        .takeUntilEval(stop.get)
        .guarantee(stopped.complete(()))
        .toPekkoSourceTask
        .flatMap(pekkoChunks =>
          sendReceive(
            HttpRequest(POST, uri.asPekko, AcceptJson,
              HttpEntity.Chunked(`application/x-ndjson`.toContentType, pekkoChunks)),
            logData = Some("postObservable")))
        .flatMap(unmarshal[B](POST, uri))
        .pipeIf(terminateStreamOnCancel)(_.doOnCancel(
          // Terminate stream properly to avoid "TCP Connection reset" error
          // Maybe a race condition. So good luck!
          stop.complete(()) *> stopped.get))

  @TestOnly
  final def postObservableJsonString(uri: Uri, data: Observable[String])(implicit s: Task[Option[SessionToken]]): Task[Json] =
    data
      .map(o => ByteString(o) ++ LF)
      .chunk(chunkSize)
      .map(Chunk(_))
      .append(LastChunk)
      .toPekkoSourceTask
      .flatMap(pekkoChunks =>
        sendReceive(
          HttpRequest(POST, uri.asPekko, AcceptJson,
            HttpEntity.Chunked(`application/x-ndjson`.toContentType, pekkoChunks)),
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
    post_[A](uri, data, AcceptJson)
      .flatMap { httpResponse =>
        Task.defer {
          if !httpResponse.status.isSuccess && !allowedStatusCodes(httpResponse.status.intValue) then
            failWithResponse(uri, httpResponse)
          else
            Task.pure(httpResponse.status.intValue)
        }.guarantee(Task {
          httpResponse.discardEntityBytes()
        })
      }

  final def post_[A: Encoder](uri: Uri, data: A, headers: List[HttpHeader])
    (implicit s: Task[Option[SessionToken]])
  : Task[HttpResponse] =
    for
      // Maybe executeOn avoid blocking with a single thread Scheduler,
      // but sometimes throws RejectedExecutionException in test build
      //entity <- Task.deferFuture(executeOn(materializer.executionContext)(implicit ec => Marshal(data).to[RequestEntity]))
      entity <- Task.deferFutureAction(implicit s => Marshal(data).to[RequestEntity])
      response <- sendReceive(
        HttpRequest(POST, uri.asPekko, headers, entity),
        logData = Some(data.toString))
    yield response

  final def postRaw(uri: Uri, headers: List[HttpHeader], entity: RequestEntity)
    (implicit s: Task[Option[SessionToken]])
  : Task[HttpResponse] =
    sendReceive(HttpRequest(POST, uri.asPekko, headers, entity))

  final def sendReceive(request: HttpRequest, logData: => Option[String] = None)
    (implicit sessionTokenTask: Task[Option[SessionToken]])
  : Task[HttpResponse] =
    withCheckedAgentUri(request)(request =>
      sessionTokenTask.flatMap { sessionToken =>
        if closed then {
          logger.debug(s"(WARN) PekkoHttpClient has actually been closed: ${requestToString(request, logData)}")
        }
        val number = requestCounter.incrementAndGet()
        val headers = sessionToken.map(token => `x-js7-session`(token)).toList :::
          `x-js7-request-id`(number) ::
          CorrelId.current.toOption.map(`x-js7-correlation-id`(_)).toList :::
          request.headers.toList :::
          standardHeaders
        val req = request.withHeaders(headers).pipeIf(useCompression)(encodeGzip)
        @volatile var canceled = false
        var responseFuture: Future[HttpResponse] = null
        val since = now
        lazy val logPrefix = s"#$number$nameString${sessionToken.fold("")(o => " " + o.short)}"
        lazy val responseLog0 = s"$logPrefix ${requestToString(req, logData, isResponse = true)} "
        def responseLogPrefix = responseLog0 + since.elapsed.pretty
        logger.trace(s">-->  $logPrefix ${requestToString(req, logData)}")
        Task
          .deferFutureAction { scheduler =>
            responseFuture = http.singleRequest(req,
              if req.uri.scheme == "https" then httpsConnectionContext else http.defaultClientHttpsContext)
            responseFuture
              .recover { case t if canceled =>
                logger.trace(s"$logPrefix Ignored after cancel: ${t.toStringWithCauses}")
                // Task guarantee below may report a failure after cancel
                // via thread pools's reportFailure. To avoid this, we convert the failure
                // to a dummy successful response, which will get lost immediately.
                HttpResponse(GatewayTimeout, entity = "CANCELED")
              }(scheduler)
          }
          .onErrorRecoverWith {
            case t: pekko.stream.StreamTcpException => Task.raiseError(makePekkoExceptionLegible(t))
          }
          .map(decompressResponse)
          .pipeIf(logger.underlying.isDebugEnabled)(
            logResponding(request, _, responseLogPrefix)
              .map { response =>
                lazy val prefix = "#" + number
                if !logger.underlying.isTraceEnabled then
                  response
                else
                  logResponseStream(response, prefix)
              })
          .guaranteeCase {
            case ExitCase.Canceled => Task {
              canceled = true
              logger.debug(s"<~~ ⚫️$responseLogPrefix => canceled")
              if responseFuture != null then {
                // TODO Pekko's max-open-requests may be exceeded when new requests are opened
                //  while many canceled requests are still not completed by Pekko
                //  until the server has reponded or some Pekko (idle connection) timeout.
                //  Anyway, the caller's code should be fault-tolerant.
                // TODO Maybe manage own connection pool? Or switch to http4s?
                executeOn(materializer.executionContext) { implicit ec =>
                  responseFuture
                    .flatMap(_
                      .discardEntityBytes().future.andThen { case tried =>
                        logger.debug(s"    🗑️ $responseLogPrefix discardEntityBytes => " +
                          tried.fold(_.toStringWithCauses, _ => "ok"))
                      })
                    .map((_: Done) => ())
                    .recover { case _ => () }
                }
              }
            }

            case ExitCase.Error(throwable) => Task.defer {
              val sym = throwable match {
                case _: java.net.ConnectException => "⭕"
                case _: pekko.stream.scaladsl.TcpIdleTimeoutException => "🔥"
                case t: pekko.stream.StreamTcpException
                  if t.getMessage.contains("java.net.ConnectException: ") => "⭕"
                case t: LegiblePekkoHttpException
                  if t.getMessage.contains("java.net.ConnectException: ") => "⭕"
                case _ => "💥"
              }
              logger.debug(
                s"<~~ $sym$responseLogPrefix => failed with ${throwable.toStringWithCauses}")
              Task.raiseError(toPrettyProblem(throwable).throwable)
            }

            case ExitCase.Completed => Task.unit
          }
      })

  private def logResponding(
    request: HttpRequest,
    untilResponded: Task[HttpResponse],
    responseLogPrefix: => String)
  : Task[HttpResponse] =
    if request.headers.contains(StreamingJsonHeader) then
      untilResponded.map { response =>
        logResponse(response, responseLogPrefix, " ✔")
        response
      }
    else
      var waitingLogged = false
      untilResponded
        .whenItTakesLonger()(_ => Task {
          val sym = if !waitingLogged then "🟡" else "🟠"
          waitingLogged = true
          logger.debug(
            s"... $sym$responseLogPrefix => Still waiting for response${closed ?? " (closed)"}")
        })
        .flatTap(response => Task(
          logResponse(response, responseLogPrefix, if waitingLogged then "🔵" else " ✔")
        ))

  private def logResponse(response: HttpResponse, responseLogPrefix: String, good: String): Unit =
    val sym =
      if !response.status.isFailure then
        good
      else response.status match
        case Unauthorized => "⛔"
        case Forbidden =>
          response.entity match
            case HttpEntity.Strict(`application/json`, bytes)
              if (bytes.parseJsonAs[Problem].exists(_ is InvalidSessionTokenProblem)) =>
              "🔒" // The SessionToken has probably expired. Then the caller will re-login.
            case _ =>
              "⛔"
        case _ => "❓"

    val suffix = response.status.isFailure ??
      (try response.entity match {
        case HttpEntity.Strict(`application/json`, bytes) =>
          bytes.parseJsonAs[Problem].toOption.fold("")(" · " + _)

        case HttpEntity.Strict(ContentType.WithCharset(`text/plain`, charset), bytes) =>
          bytes.decodeString(charset.nioCharset)
            .parseJsonAs[Problem].toOption.fold("")(" · " + _)

        case _ => ""
      } catch {
        case NonFatal(_) => ""
      })
    val arrow = if response.entity.isChunked then "<-<-" else "<--<"
    logger.debug(s"$arrow$sym$responseLogPrefix => ${response.status}$suffix")

  // >-->  request
  // <--<  non-chunked response
  // <-<- ✔header of chunked response
  // <-<-  chunk
  // <--|  last chunk

  private def logResponseStream(response: HttpResponse, responseLogPrefix: => String)
  : HttpResponse =
    response.entity match
      case chunked: Chunked =>
        val isUtf8 = (chunked.contentType.charsetOption.contains(`UTF-8`)
          || chunked.contentType.mediaType.toString == `application/x-ndjson`.toString)
        response.pipeIf(logger.underlying.isTraceEnabled)(_
          .withEntity(chunked.copy(
            chunks = chunked.chunks
              .map { chunk =>
                if chunk.isLastChunk then {
                  val arrow = if chunk.isLastChunk then "<--|  " else "<-<-  "
                  val data =
                    if isUtf8 then
                      chunk.data.utf8String.truncateWithEllipsis(
                        200, showLength = true, firstLineOnly = true, quote = true)
                    else
                      s"${chunk.data.length} bytes"
                  logger.trace(s"$arrow$responseLogPrefix $data")
                }
                chunk
              })))
      case _ => response

  private def unmarshal[A: FromResponseUnmarshaller](method: HttpMethod, uri: Uri)(httpResponse: HttpResponse) =
    if !httpResponse.status.isSuccess then
      failWithResponse(uri, httpResponse)
    else
      Task.deferFuture[A](
        executeOn(materializer.executionContext) { implicit ec =>
          Unmarshal(httpResponse).to[A]
            .recover { case t =>
              if !materializer.isShutdown then {
                logger.debug(
                  s"💥 $toString: Error when unmarshalling response of ${method.name} $uri: ${
                    t.toStringWithCauses}", t)
              }
              throw t
            }
        })

  private def failWithResponse(uri: Uri, response: HttpResponse): Task[Nothing] =
    response.entity.asUtf8String.flatMap(errorMsg =>
      Task.raiseError(new HttpException(POST, uri, response, errorMsg)))

  private def withCheckedAgentUri[A](request: HttpRequest)(body: HttpRequest => Task[A]): Task[A] =
    toCheckedAgentUri(request.uri.asUri) match
      case Left(problem) => Task.raiseError(problem.throwable)
      case Right(uri) => body(request.withUri(uri.asPekko))

  private[http] final def toCheckedAgentUri(uri: Uri): Checked[Uri] =
    checkAgentUri(normalizeAgentUri(uri))

  private[http] final def normalizeAgentUri(uri: Uri): Uri =
    val PekkoUri(scheme, authority, path, query, fragment) = uri.asPekko
    if scheme.isEmpty && authority.isEmpty then
      PekkoUri(basePekkoUri.scheme, basePekkoUri.authority, path, query, fragment).asUri
    else
      uri

  /** Checks `uri` againts `baseUri` - scheme and authority must be equal. */
  private[http] final def checkAgentUri(uri: Uri): Checked[Uri] =
    val pekkoUri = uri.asPekko
    if pekkoUri.scheme == basePekkoUri.scheme &&
      pekkoUri.authority == basePekkoUri.authority &&
      pekkoUri.path.toString.startsWith(uriPrefixPath) then
      Right(uri)
    else
      Left(Problem(s"URI '$uri' does not match $baseUri$uriPrefixPath"))

  private def requestToString(request: HttpRequest, logData: => Option[String], isResponse: Boolean = false) =
    val b = new StringBuilder(300)
    b.append(request.method.value.pipeIf(isResponse)(_.toLowerCase(Locale.ROOT)))
    b.append(' ')
    b.append(request.uri)
    if !request.entity.isKnownEmpty then
      for o <- logData do
        b.append(' ')
        b.append(o.truncateWithEllipsis(200, showLength = true, firstLineOnly = true))
    b.toString

  override def hasRelevantStackTrace(throwable: Throwable): Boolean =
    PekkoHttpClient.hasRelevantStackTrace(throwable)

  private def makePekkoExceptionLegible(t: pekko.stream.StreamTcpException): RuntimeException =
    pekkoExceptionRegex.findFirstMatchIn(t.toString)
      .toList
      .flatMap(_.subgroups)
      .match
        case List(m1, m2) => new LegiblePekkoHttpException(s"$name $m1): $m2", t)
        case _ => t

  override def toString = s"$baseUri$nameString"

  private lazy val nameString = name.nonEmpty ?? s" »$name«"


object PekkoHttpClient:
  private val EmptyLine = ByteArray("\n")

  def resource(
    uri: Uri,
    uriPrefixPath: String,
    httpsConfig: HttpsConfig = HttpsConfig.empty,
    name: String = "")
    (implicit actorSystem: ActorSystem)
  : Resource[Task, PekkoHttpClient] =
    Resource.fromAutoCloseable(Task(new PekkoHttpClient.Standard(
      uri, uriPrefixPath = uriPrefixPath, actorSystem, httpsConfig, name = name)))
    //Resource.make(
    //  acquire = Task(new PekkoHttpClient.Standard(
    //    uri, uriPrefixPath = uriPrefixPath, actorSystem, httpsConfig, name = name)))(
    //  release = { client =>
    //    val logout = client match {
    //      case client: HttpSessionApi => client.logout()
    //      case _ => Task.unit
    //    }
    //    logout
    //      .guarantee(Task(client.close()))
    //  })

  final case class `x-js7-session`(sessionToken: SessionToken)
  extends ModeledCustomHeader[`x-js7-session`]:
    val companion = `x-js7-session`
    def value = sessionToken.secret.string
    val renderInRequests = true
    val renderInResponses = false
  object `x-js7-session` extends ModeledCustomHeaderCompanion[`x-js7-session`]:
    val name = "x-js7-session"
    def parse(value: String) = Success(new `x-js7-session`(SessionToken(SecretString(value))))

  final case class `x-js7-request-id`(number: Long)
  extends ModeledCustomHeader[`x-js7-request-id`]:
    val companion = `x-js7-request-id`
    def value = "#" + number
    val renderInRequests = true
    val renderInResponses = false
  object `x-js7-request-id` extends ModeledCustomHeaderCompanion[`x-js7-request-id`]:
    val name = "x-js7-request-id"

    def parse(value: String) =
      parseNumber(value)
        .map(`x-js7-request-id`(_))
        .left.map(_.throwable)
        .toTry

    def parseNumber(value: String): Checked[Long] =
      if !value.startsWith("#") then
        Left(Problem.pure("x-js7-request-id HTTP header must start with a #"))
      else
        try Right(value.substring(1).toLong)
        catch { case t: NumberFormatException =>
          Left(Problem.pure(t.toString))
        }

  final case class `x-js7-correlation-id`(correlId: CorrelId)
  extends ModeledCustomHeader[`x-js7-correlation-id`]:
    val companion = `x-js7-correlation-id`
    // Convert to ASCII
    def value = correlId.toAscii
    val renderInRequests = true
    val renderInResponses = false
  object `x-js7-correlation-id` extends ModeledCustomHeaderCompanion[`x-js7-correlation-id`]:
    val name = "x-js7-correlation-id"
    def parse(asciiString: String) =
      CorrelId.checked(asciiString).map(`x-js7-correlation-id`(_)).asTry

  private val logger = Logger[this.type]
  private val LF = ByteString("\n")
  private val AcceptJson = Accept(MediaTypes.`application/json`) :: Nil
  private val requestCounter = AtomicLong(0)

  final class Standard(
    protected val baseUri: Uri,
    protected val uriPrefixPath: String = "",
    protected val actorSystem: ActorSystem,
    protected val httpsConfig: HttpsConfig = HttpsConfig.empty,
    protected val name: String = "")
  extends PekkoHttpClient

  private val connectionWasClosedUnexpectedly = Problem.pure("Connection was closed unexpectedly")

  private val PekkoTcpCommandRegex = """Tcp command \[([A-Za-z]+)\(([^,)]+).*""".r

  private val pekkoExceptionRegex = new Regex("org.apache.pekko.stream.StreamTcpException: Tcp command " +
    """\[(Connect\([^,]+).+\)] failed because of ([a-zA-Z.]+Exception.*)""")

  def toPrettyProblem(problem: Problem): Problem =
    problem.throwableOption.fold(problem) { throwable =>
      val pretty = toPrettyProblem(throwable)
      pretty.throwableOption match
        case Some(t) if t eq throwable =>
          problem
        case _ =>
          logger.debug(s"toPrettyProblem($problem) => $pretty")
          pretty
    }

  def toPrettyProblem(throwable: Throwable): Problem =
    def default = Problem.fromThrowable(throwable)
    throwable match
      case pekko.http.scaladsl.model.EntityStreamException(ErrorInfo(summary, _)) =>
        if summary contains "connection was closed unexpectedly" then
          connectionWasClosedUnexpectedly
        else Problem.pure(summary)

      case t if t.getClass.getName endsWith "UnexpectedConnectionClosureException" =>
        connectionWasClosedUnexpectedly

      case t: pekko.stream.StreamTcpException =>
        // TODO Long longer active since makePekkoExceptionLegible?
        t.getMessage match
          case PekkoTcpCommandRegex(command, host_) =>
            val host = host_.replace("/<unresolved>", "")
            val prefix = s"TCP $command $host"
            t.getCause match
              case t: java.net.SocketException =>
                Problem(prefix + ": " + t.getMessage)
              case t: java.net.UnknownHostException =>
                Problem(prefix + ": " + t.toString stripPrefix "java.net.")
              case _ => default
          case _ => default

      case _ => default

  final class HttpException private[http](
    method: HttpMethod,
    val uri: Uri,
    httpResponse: HttpResponse,
    val dataAsString: String)
  extends HttpClient.HttpException, NoStackTrace:
    def statusInt = status.intValue

    // Don't publish httpResponse because its entity stream has already been consumed for dataAsString
    def status: StatusCode = httpResponse.status

    def header[A >: Null <: HttpHeader: ClassTag]: Option[A] = httpResponse.header[A]

    override def toString = getMessage

    override def getMessage =
      s"$prefixString => ${problem getOrElse shortDataString}"

    private def prefixString =
      s"HTTP ${httpResponse.status}: ${method.value} $uri"

    private def shortDataString = dataAsString.truncateWithEllipsis(10000)

    lazy val problem: Option[Problem] =
      if httpResponse.entity.contentType == `application/json` then
        io.circe.parser.decode[Problem](dataAsString) match
          case Left(error) =>
            logger.debug(s"$uri: $prefixString, Problem cannot be parsed: $error - $dataAsString")
            None
          case Right(o) => Some(o)
      else
        None

  def hasRelevantStackTrace(throwable: Throwable): Boolean =
    throwable != null && throwable.getStackTrace.nonEmpty &&
      (throwable match {
        case _: pekko.stream.StreamTcpException => false
        case _: java.net.SocketException => false
        case _ => true
      })

  private final class LegiblePekkoHttpException(
    message: String,
    cause: pekko.stream.StreamTcpException)
  extends RuntimeException(message, cause), NoStackTrace:
    override def toString = getMessage