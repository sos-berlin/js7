package js7.common.http

import cats.effect.{Deferred, IO, Outcome, Resource, ResourceIO}
import cats.syntax.applicativeError.*
import fs2.Stream
import io.circe.syntax.*
import io.circe.{Decoder, Encoder, Json}
import java.util.Locale
import java.util.concurrent.atomic.AtomicLong
import js7.base.auth.SessionToken
import js7.base.catsutils.CatsEffectExtensions.*
import js7.base.circeutils.CirceUtils.{RichCirceString, RichJson}
import js7.base.configutils.Configs.RichConfig
import js7.base.data.ByteArray
import js7.base.data.ByteSequence.ops.*
import js7.base.exceptions.HasIsIgnorableStackTrace
import js7.base.fs2utils.StreamExtensions.*
import js7.base.generic.SecretString
import js7.base.io.https.Https.loadSSLContext
import js7.base.io.https.HttpsConfig
import js7.base.log.Logger.syntax.*
import js7.base.log.{CorrelId, Logger}
import js7.base.monixlike.MonixLikeExtensions.onErrorTap
import js7.base.problem.Checked.*
import js7.base.problem.Problems.InvalidSessionTokenProblem
import js7.base.problem.{Checked, Problem, ProblemException}
import js7.base.time.ScalaTime.*
import js7.base.time.Stopwatch.bytesPerSecondString
import js7.base.utils.Atomic.extensions.*
import js7.base.utils.CatsUtils.syntax.whenItTakesLonger
import js7.base.utils.Missing.*
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.utils.SystemPropertiesExtensions.asSwitch
import js7.base.utils.{Atomic, LineSplitterPipe, Missing}
import js7.base.web.{HttpClient, Uri}
import js7.common.http.JsonStreamingSupport.{StreamingJsonHeader, StreamingJsonHeaders, `application/x-ndjson`}
import js7.common.http.PekkoHttpClient.*
import js7.common.http.PekkoHttpUtils.{RichPekkoAsUri, RichPekkoUri, RichResponseEntity, decompressResponse, encodeGzip}
import js7.common.http.StreamingSupport.{asFs2Stream, toPekkoSourceResource}
import js7.common.pekkohttp.ByteSequenceStreamExtensions.*
import js7.common.pekkohttp.CirceJsonSupport.{jsonMarshaller, jsonUnmarshaller}
import js7.common.pekkoutils.ByteStrings.syntax.*
import js7.common.pekkoutils.PekkoForExplicitNulls.header3
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
import org.apache.pekko.http.scaladsl.model.{ContentType, ErrorInfo, HttpEntity, HttpHeader, HttpMessage, HttpMethod, HttpRequest, HttpResponse, MediaTypes, RequestEntity, StatusCode, Uri as PekkoUri}
import org.apache.pekko.http.scaladsl.unmarshalling.{FromResponseUnmarshaller, Unmarshal}
import org.apache.pekko.http.scaladsl.{ConnectionContext, Http}
import org.apache.pekko.stream.Materializer
import org.apache.pekko.util.ByteString
import org.jetbrains.annotations.TestOnly
import scala.collection.immutable.Map.Map2
import scala.concurrent.TimeoutException
import scala.concurrent.duration.Deadline.now
import scala.concurrent.duration.FiniteDuration
import scala.reflect.ClassTag
import scala.util.control.{NoStackTrace, NonFatal}
import scala.util.matching.Regex
import scala.util.{Success, Try}

/** General HTTP client.
 * <p>
 *   Supports streaming, JSON, logging.
 * <h3>Meaning of logged arrows</h3>
 * Looks better with Fire Code font.
 * <p>Request
 * <br><code>>-->  </code> chunked request HTTP header
 * <br><code>->->  </code> chunk of request stream
 * <br><code>~~> 💥</code> error while preparing a chunk of a request
 * <br><code>|-->  </code> last chunk of request stream or
 * <br><code>|-->  </code> non-chunked request
 * <p>Response
 * <br><code>&lt;--&lt;✔</code> HTTP header of chunked response, ok
 * <br><code>&lt;-&lt;-  </code> chunk of response stream
 * <br><code>&lt;--| </code> last chunk of response stream
 * <br><code>&lt;~~ 💥</code> error while receiving chunks
 * <br><code>&lt;--|✔</code> non-chunked response, ok
 * <p>Cancellation
 * <br><code>🗑 &nbsp; ↘</code> start of response cancellation
 * <br><code>🗑 &nbsp; ↙</code> end of response cancellation
 * <br><code>&lt;~~ ◼️</code> canceled
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
  private lazy val chunkSize = http.system.settings.config.memorySizeAsInt("js7.web.chunk-size").orThrow
  private final lazy val httpPrefetch = actorSystem.settings.config.getInt("js7.web.client.prefetch")
  private final lazy val jsonPrefetch = actorSystem.settings.config.getInt("js7.web.server.prefetch")
  @volatile private var closed = false

  protected def httpsConfig: HttpsConfig

  private lazy val httpsConnectionContext =
    if httpsConfig.keyStoreRef.isEmpty && httpsConfig.trustStoreRefs.isEmpty then
      http.defaultClientHttpsContext
    else
      ConnectionContext.httpsClient(
        loadSSLContext(httpsConfig.keyStoreRef, httpsConfig.trustStoreRefs))

  final def materializer: Materializer = implicitly[Materializer]

  def close(): Unit =
    logger.trace(s"$toString: close")
    closed = true

  final def getDecodedLinesStream[A: Decoder](
    uri: Uri,
    responsive: Boolean = false,
    returnHeartbeatAs: Option[ByteArray] = None,
    idleTimeout: Option[FiniteDuration] = None,
    prefetch: Int | Missing = Missing,
    dontLog: Boolean = false)
    (using IO[Option[SessionToken]])
  : IO[Stream[IO, A]] =
    val heartbeatAsChunk = fs2.Chunk.fromOption(returnHeartbeatAs)
    val myPrefetch = prefetch getOrElse this.httpPrefetch: Int
    getRawLinesStream(uri, returnHeartbeatAs, idleTimeout = idleTimeout, dontLog = dontLog).map:
      _.map:
        case HttpHeartbeatByteArray => heartbeatAsChunk
        case o => fs2.Chunk.singleton(o)
      .unchunks
      .mapParallelBatch(prefetch = myPrefetch):
        _.parseJsonAs[A].orThrow

  final def getRawLinesStream(
    uri: Uri,
    returnHeartbeatAs: Option[ByteArray] = None,
    idleTimeout: Option[FiniteDuration] = None,
    dontLog: Boolean = false)
    (using IO[Option[SessionToken]])
  : IO[Stream[IO, ByteArray]] =
    val heartbeatAsChunk = fs2.Chunk.fromOption(returnHeartbeatAs)
    get_[HttpResponse](uri, StreamingJsonHeaders, dontLog = dontLog)
      .map(_
        .entity.withoutSizeLimit.dataBytes
        .pipeMaybe(idleTimeout): (stream, timeout) =>
          // Because the FS2/Pekko bridge appears not to be cancelable, we use
          // Pekko's idleTimeout instead of FS2's timeoutOnPull
          stream.idleTimeout(timeout).recoverWith:
            case _: TimeoutException =>
              pekko.stream.scaladsl.Source.failed(IdleTimeoutException(uri, timeout))
        .asFs2Stream()
        .pipeIf(logger.isDebugEnabled):
          _.logTiming(_.size, (d, n, _) => IO:
            if d >= 1.s && n >= 10_000_000 then
              logger.debug(s"get $uri: ${bytesPerSecondString(d, n)}"))
        .through:
          LineSplitterPipe()
        // See above Pekko's stream.idleTimout
        //.pipeMaybe(idleTimeout): (stream, t) =>
        //  stream.timeoutOnPullTo(t, Stream.raiseError[IO](IdleTimeoutException(uri, t)))
        .map:
          case HttpHeartbeatByteArray => heartbeatAsChunk
          case o => fs2.Chunk.singleton(o)
        .unchunks
        .recoverWith:
          ignoreIdleTimeout orElse endStreamOnNoMoreElementNeeded)
      .recover(ignoreIdleTimeout)

  private def endStreamOnNoMoreElementNeeded: PartialFunction[Throwable, Stream[IO, Nothing]] =
    case t @ pekko.stream.SubscriptionWithCancelException.NoMoreElementsNeeded =>
      // See also pekko.http.server.stream-cancellation-delay
      // On NoMoreElementsNeeded the Stream ends silently !!! Maybe harmless?
      logger.warn(s"Ignore $t")
      if hasRelevantStackTrace(t) then logger.debug(s"Ignore $t", t)
      Stream.empty

  private def ignoreIdleTimeout: PartialFunction[Throwable, Stream[IO, Nothing]] =
    case t: pekko.stream.scaladsl.TcpIdleTimeoutException =>
      // Idle timeout is ignored !!!
      // We issue a warning, because the caller should make this not happen.
      logger.warn(s"Ignore $t")
      if hasRelevantStackTrace(t) then logger.debug(s"Ignore $t", t)
      Stream.empty

  /** HTTP Get with Accept: application/json. */
  final def get[A: Decoder](uri: Uri, dontLog: Boolean = false)(using IO[Option[SessionToken]])
  : IO[A] =
    getWithHeaders[A](uri, Nil, dontLog = dontLog)

  /** HTTP Get with Accept: application/json. */
  final def getWithHeaders[A: Decoder](uri: Uri, headers: List[HttpHeader], dontLog: Boolean = false)
    (using IO[Option[SessionToken]])
  : IO[A] =
    get_[A](uri, AcceptJson ::: headers, dontLog = dontLog)

  final def get_[A: FromResponseUnmarshaller](
    uri: Uri,
    headers: List[HttpHeader] = Nil,
    dontLog: Boolean = false)
    (using IO[Option[SessionToken]])
  : IO[A] =
    sendReceive(
      HttpRequest(GET, PekkoUri(uri.string), `Cache-Control`(`no-cache`, `no-store`) :: headers),
      dontLog = dontLog
    ).flatMap(unmarshal[A](GET, uri))

  final def post[A: Encoder, B: Decoder](uri: Uri, data: A, dontLog: Boolean = false)(
    using IO[Option[SessionToken]])
  : IO[B] =
    post2[A, B](uri, data, Nil, dontLog = dontLog)

  final def postStream[A: Encoder, B: Decoder](
    uri: Uri,
    stream: Stream[IO, A],
    responsive: Boolean = false,
    terminateStreamOnCancel: Boolean = false,
    dontLog: Boolean = false)
    (using IO[Option[SessionToken]])
  : IO[B] =
    def toNdJson(a: A): ByteString = a.asJson.toByteSequence[ByteString] ++ LF

    val chunks: Stream[IO, ByteString] =
      //if responsive then
      //  for a <- stream yield toNdJson(a)
      //else
        stream
          .mapParallelBatch(prefetch = jsonPrefetch): a =>
            toNdJson(a).chunkStream(chunkSize).toVector
          .map(fs2.Chunk.from)
          .unchunks

    IO.defer:
      val stop = Deferred.unsafe[IO, Unit]
      val stopped = Deferred.unsafe[IO, Unit]
      chunks
        .map(Chunk(_))
        .append[IO, ChunkStreamPart](Stream.emit(LastChunk))
        .interruptWhenF(stop.get)
        .onFinalize:
          stopped.complete(()).void
        .toPekkoSourceResource
        .use: pekkoChunks =>
          sendReceive(
            HttpRequest(POST, uri.asPekko, AcceptJson,
              HttpEntity.Chunked(`application/x-ndjson`.toContentType, pekkoChunks)),
            logData = Some("postStream"),
            dontLog = dontLog)
        .flatMap(unmarshal[B](POST, uri))
        .pipeIf(terminateStreamOnCancel)(_.onCancel:
          // Terminate stream properly to avoid "TCP Connection reset" error
          // Maybe a race condition. So good luck!
          stop.complete(()) *> stopped.get)

  @TestOnly
  final def postJsonStringStream(uri: Uri, data: Stream[IO, String])
    (using IO[Option[SessionToken]])
  : IO[Json] =
    data
      .map(o => ByteString(o) ++ LF)
      .splitByteSequences(chunkSize)
      .map(Chunk(_))
      .append(Stream.emit(LastChunk))
      .toPekkoSourceResource
      .use: pekkoChunks =>
        sendReceive(
          HttpRequest(POST, uri.asPekko, AcceptJson,
            HttpEntity.Chunked(`application/x-ndjson`.toContentType, pekkoChunks)),
          logData = Some("postStream"),
          dontLog = false)
      .flatMap(unmarshal[Json](POST, uri))

  @TestOnly
  final def postWithHeaders[A: Encoder, B: Decoder](
    uri: Uri,
    data: A,
    headers: List[HttpHeader],
    dontLog: Boolean = false)
    (using IO[Option[SessionToken]])
  : IO[B] =
    post2[A, B](uri, data, headers, dontLog = dontLog)

  private def post2[A: Encoder, B: Decoder](
    uri: Uri,
    data: A,
    headers: List[HttpHeader],
    dontLog: Boolean)
    (using IO[Option[SessionToken]])
  : IO[B] =
    post_[A](uri, data, AcceptJson ::: headers, dontLog = dontLog)
      .flatMap(unmarshal[B](POST, uri))

  final def postDiscardResponse[A: Encoder](
    uri: Uri,
    data: A,
    allowedStatusCodes: Set[Int] = Set.empty)
    (using IO[Option[SessionToken]])
  : IO[Int] =
    post_[A](uri, data, AcceptJson)
      .flatMap: httpResponse =>
        IO.defer:
          if !httpResponse.status.isSuccess && !allowedStatusCodes(httpResponse.status.intValue) then
            failWithResponse(uri, httpResponse)
          else
            IO.pure(httpResponse.status.intValue)
        .guarantee(IO:
          httpResponse.discardEntityBytes())

  final def post_[A: Encoder](
    uri: Uri,
    data: A,
    headers: List[HttpHeader],
    dontLog: Boolean = false)
    (implicit s: IO[Option[SessionToken]])
  : IO[HttpResponse] =
    for
      // Maybe executeOn avoid blocking with a single thread Scheduler,
      // but sometimes throws RejectedExecutionException in test build
      //entity <- IO.deferFuture(executeOn(materializer.executionContext)(implicit ec => Marshal(data).to[RequestEntity]))
      entity <- IO.fromFutureWithEC(implicit ec => IO:
        Marshal(data).to[RequestEntity])
      response <- sendReceive(
        HttpRequest(POST, uri.asPekko, headers, entity),
        logData = Some(data.toString),
        dontLog = dontLog)
    yield response

  @TestOnly
  final def postRaw(
    uri: Uri,
    headers: List[HttpHeader],
    entity: RequestEntity,
    dontLog: Boolean = false)
    (using IO[Option[SessionToken]])
  : IO[HttpResponse] =
    sendReceive(HttpRequest(POST, uri.asPekko, headers, entity), dontLog = dontLog)

  final def sendReceive(request: HttpRequest, logData: => Option[String] = None, dontLog: Boolean)
    (implicit sessionTokenIO: IO[Option[SessionToken]])
  : IO[HttpResponse] =
   HttpMXBeanUtils.clientRequestResource.surround:
    withCheckedAgentUri(request): request =>
      sessionTokenIO.flatMap: sessionToken =>
        if closed then
          logger.debug(s"(WARN) PekkoHttpClient has actually been closed: ${requestToString(request, logData)}")
        val number = requestCounter.incrementAndGet()

        val req =
          logStream("#" + number, "->->  ", "|-->  ", "~~> 💥", dontLog = dontLog):
            val headers = sessionToken.map(token => `x-js7-session`(token)).toList :::
              `x-js7-request-id`(number) ::
              CorrelId.current.toOption.map(`x-js7-correlation-id`(_)).toList :::
              request.headers.toList :::
              standardHeaders
            request.withHeaders(headers).pipeIf(useCompression)(encodeGzip)

        @volatile var canceled = false
        //var responseFuture: Future[HttpResponse] = null
        val since = now
        lazy val logPrefix = s"#$number$nameString${sessionToken.fold("")(o => " " + o.short)}"
        lazy val responseLog0 = s"$logPrefix ${requestToString(req, logData, isResponse = true)} "
        def responseLogPrefix = responseLog0 + since.elapsed.pretty
        if !dontLog then
          def arrow = if request.entity.isChunked then ">-->" else "|-->"
          logger.trace(s"$arrow  $logPrefix ${requestToString(req, logData)}")
        IO
          .fromFutureCancelable:
            IO:
              val httpsCtx = req.uri.scheme match
                case "https" => httpsConnectionContext
                case _ => http.defaultClientHttpsContext
              //————————————————————————————————————————————
              val future = http.singleRequest(req, httpsCtx)
              //————————————————————————————————————————————
              val cancel = IO.executionContext.flatMap(implicit ec => IO:
                logger.debug(s"🗑   ↘ $responseLogPrefix cancel ...")
                // TODO Pekko's max-open-requests may be exceeded when new requests are opened
                //  while many canceled requests are still not completed by Pekko
                //  until the server has responded or some Pekko (idle connection) timed out.
                //  Anyway, the caller's code should be fault-tolerant.
                // TODO Maybe manage own connection pool? Or switch to http4s?
                future
                  .flatMap(_
                    .discardEntityBytes().future)
                  .andThen: tried =>
                    logger.debug(s"🗑   ↙ $responseLogPrefix canceled with discardEntityBytes => " +
                      tried.fold(_.toStringWithCauses, _ => "OK"))
                  .map((_: Done) => ())
                  .handleError(_ => ())
                ()) // Forget the started Future, discard the remaining entities in background

              future -> cancel
          .recoverWith:
            case t if canceled => IO:
              logger.trace(s"$logPrefix Ignored after cancel: ${t.toStringWithCauses}")
              // IO guarantee below may report a failure after cancel
              // via thread pools's reportFailure. To avoid this, we convert the failure
              // to a dummy successful response, which will get lost immediately.
              HttpResponse(GatewayTimeout, entity = "CANCELED")
            case t: pekko.stream.StreamTcpException => IO.raiseError(simplifyPekkoException(t))
            case t: Throwable => IO.raiseError(toPrettyThrowable(t))
          .map(decompressResponse)
          .pipeIf(logger.isDebugEnabled):
            logResponding(request, _, responseLogPrefix)
              .map: response =>
                logStream("#" + number, "<-<-  ", "<--|  ", "<~~ 💥", dontLog = dontLog):
                  response
          .guaranteeCaseLazy:
            case Outcome.Canceled() => IO:
              canceled = true
              logger.debug(s"<~~ ◼️ $responseLogPrefix => canceled")

            case Outcome.Errored(throwable) => IO.defer:
              val sym = throwable match
                case _: java.net.ConnectException => "⭕ "
                case _: pekko.stream.scaladsl.TcpIdleTimeoutException => "🔥 "
                case t: pekko.stream.StreamTcpException
                  if t.getMessage.contains("java.net.ConnectException: ") => "⭕ "
                case t: SimplifiedPekkoHttpException
                  if t.getMessage.contains("java.net.ConnectException: ") => "⭕ "
                case _ => "💥 "
              IO(logger.debug:
                s"<~~ $sym$responseLogPrefix => failed with ${throwable.toStringWithCauses}")

            case Outcome.Succeeded(_) => IO.unit

  private def logResponding(
    request: HttpRequest,
    untilResponded: IO[HttpResponse],
    responseLogPrefix: => String)
  : IO[HttpResponse] =
    if request.headers.contains(StreamingJsonHeader) then
      untilResponded.map: response =>
        logResponse(response, responseLogPrefix, " ✔")
        response
    else
      var waitingLogged = false
      untilResponded
        .whenItTakesLonger()(_ => IO:
          val sym = if !waitingLogged then "🟡" else "🟠"
          waitingLogged = true
          logger.debug:
            s"... $sym$responseLogPrefix => Still waiting for response${closed ?? " (closed)"}")
        .flatTap(response => IO:
          logResponse(response, responseLogPrefix, if waitingLogged then "🔵" else " ✔"))

  private def logResponse(response: HttpResponse, responseLogPrefix: String, good: String): Unit =
    val sym =
      if !response.status.isFailure then
        good
      else response.status match
        case Unauthorized => "⛔"
        case Forbidden =>
          response.entity match
            case HttpEntity.Strict(`application/json`, bytes)
              if bytes.parseJsonAs[Problem].exists(_ is InvalidSessionTokenProblem) =>
              "🔒" // The SessionToken has probably expired. Then the caller will re-login.
            case _ =>
              "⛔"
        case _ => "❓"

    val suffix = response.status.isFailure ?? locally:
      try response.entity match
        case HttpEntity.Strict(`application/json`, bytes) =>
          bytes.parseJsonAs[Problem].toOption.fold("")(" · " + _)

        case HttpEntity.Strict(ContentType.WithCharset(`text/plain`, charset), bytes) =>
          bytes.decodeString(charset.nioCharset)
            .parseJsonAs[Problem].toOption.fold("")(" · " + _)

        case _ => ""
      catch case NonFatal(_) => ""
    val arrow = if response.entity.isChunked then "<--<" else "<--|"
    logger.debug(s"$arrow$sym$responseLogPrefix => ${response.status}$suffix")

  private def logStream[M <: HttpMessage](
    prefix: => String,
    msgArrow: String, lastArrow: String, errArrow: String, dontLog: Boolean)
    (message: M)
  : M =
    val byteTotal: AtomicLong =
      if message.isRequest then
        HttpMXBean.Bean.clientSentByteTotal
      else
        HttpMXBean.Bean.clientReceivedByteTotal
    message.entity match
      case chunked: Chunked =>
        lazy val isJson = chunked.contentType.mediaType.toString == `application/x-ndjson`.toString
        lazy val isUtf8 = isJson || chunked.contentType.charsetOption.contains(`UTF-8`)
        message.withEntity:
          chunked.copy(
            chunks =
              chunked.chunks.map: chunk =>
                byteTotal += chunk.data.size
                if !dontLog && !logger.isTraceEnabled then
                  def arrow = if chunk.isLastChunk then lastArrow else msgArrow
                  def string =
                    if isUtf8 then
                      chunk.data.utf8String.truncateWithEllipsis(
                        200, showLength = true, firstLineOnly = true, quote = true)
                    else
                      s"${chunk.data.length} bytes"

                  if isJson && chunk.data == HttpHeartbeatByteString then
                    if logHeartbeat then
                      logger.trace(s"$arrow$prefix $string 🩶")
                  else
                    logger.trace(s"$arrow$prefix $string")
                chunk
              .mapError: t =>
                logger.trace(s"$errArrow$prefix ${t.toStringWithCauses}")
                t)
        .asInstanceOf[M]

      case entity: HttpEntity.Strict =>
        byteTotal += entity.data.size
        message

      case entity: HttpEntity.Default =>
        byteTotal += entity.contentLength
        message

      case entity: HttpEntity.CloseDelimited =>
        //byteTotal += ?? —— CloseDelimited is not used
        message

  private def unmarshal[A: FromResponseUnmarshaller](method: HttpMethod, uri: Uri)(httpResponse: HttpResponse) =
    if !httpResponse.status.isSuccess then
      failWithResponse(uri, httpResponse)
    else
      IO.fromFuture[A](IO:
          Unmarshal(httpResponse).to[A])
        .onErrorTap(t => IO:
          if !materializer.isShutdown then
            logger.debug(s"💥 $toString: Error when unmarshalling response of ${
              method.name} $uri: ${t.toStringWithCauses}", t))

  private def failWithResponse(uri: Uri, response: HttpResponse): IO[Nothing] =
    response.entity.asUtf8String.flatMap: errorMsg =>
      IO.raiseError(new HttpException(POST, uri, response, errorMsg))

  private def withCheckedAgentUri[A](request: HttpRequest)(body: HttpRequest => IO[A]): IO[A] =
    toCheckedAgentUri(request.uri.asUri) match
      case Left(problem) => IO.raiseError(problem.throwable)
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
    if pekkoUri.scheme == basePekkoUri.scheme
      && pekkoUri.authority == basePekkoUri.authority
      //&& pekkoUri.path.toString.startsWith(uriPrefixPath)
    then
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

  private def simplifyPekkoException(t: pekko.stream.StreamTcpException): Throwable =
    pekkoExceptionRegex.findFirstMatchIn(t.toString)
      .toList
      .flatMap(_.subgroups)
      .match
        case List(m1, m2) => SimplifiedPekkoHttpException(s"$name $m1): $m2", t)
        case _ => toPrettyThrowable(t)

  override def toString = s"$baseUri$nameString"

  private lazy val nameString = name.nonEmpty ?? s" »$name«"


object PekkoHttpClient:
  val HttpHeartbeatByteArray: ByteArray = ByteArray("\n")
  val HttpHeartbeatByteString: ByteString = HttpHeartbeatByteArray.toByteSequence[ByteString]
  val logHeartbeat = sys.props.asSwitch("js7.log.heartbeat")

  def resource(
    uri: Uri,
    uriPrefixPath: String,
    httpsConfig: HttpsConfig = HttpsConfig.empty,
    name: String = "")
    (using actorSystem: ActorSystem)
  : ResourceIO[PekkoHttpClient] =
    Resource.fromAutoCloseable:
      IO:
        new PekkoHttpClient.Standard(
          uri, uriPrefixPath = uriPrefixPath, actorSystem, httpsConfig, name = name)

  final case class `x-js7-session`(sessionToken: SessionToken)
  extends ModeledCustomHeader[`x-js7-session`]:
    val companion: `x-js7-session`.type = `x-js7-session`
    def value: String = sessionToken.secret.string
    val renderInRequests = true
    val renderInResponses = false
  object `x-js7-session` extends ModeledCustomHeaderCompanion[`x-js7-session`]:
    val name = "x-js7-session"
    def parse(value: String): Try[`x-js7-session`] = Success(new `x-js7-session`(SessionToken(SecretString(value))))

  final case class `x-js7-request-id`(number: Long)
  extends ModeledCustomHeader[`x-js7-request-id`]:
    val companion: ModeledCustomHeaderCompanion[`x-js7-request-id`] = `x-js7-request-id`
    def value: String = "#" + number
    val renderInRequests = true
    val renderInResponses = false
  object `x-js7-request-id` extends ModeledCustomHeaderCompanion[`x-js7-request-id`]:
    val name = "x-js7-request-id"

    def parse(value: String): Try[`x-js7-request-id`] =
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
    val companion: `x-js7-correlation-id`.type = `x-js7-correlation-id`
    // Convert to ASCII
    def value: String = correlId.toAscii
    val renderInRequests = true
    val renderInResponses = false
  object `x-js7-correlation-id` extends ModeledCustomHeaderCompanion[`x-js7-correlation-id`]:
    val name = "x-js7-correlation-id"
    def parse(asciiString: String): Try[`x-js7-correlation-id`] =
      CorrelId.checked(asciiString).map(`x-js7-correlation-id`(_)).asTry

  private val logger = Logger[this.type]
  private val LF = ByteString("\n")
  private val AcceptJson = Accept(MediaTypes.`application/json`) :: Nil
  private val requestCounter = Atomic(0L)

  final class Standard(
    protected val baseUri: Uri,
    protected val uriPrefixPath: String = "",
    protected val actorSystem: ActorSystem,
    protected val httpsConfig: HttpsConfig = HttpsConfig.empty,
    protected val name: String = "")
  extends PekkoHttpClient

  private val PekkoTcpCommandRegex = """Tcp command \[([A-Za-z]+)\(([^,)]+).*""".r

  private val pekkoExceptionRegex = new Regex("org.apache.pekko.stream.StreamTcpException: Tcp command " +
    """\[(Connect\([^,]+).+\)] failed because of ([a-zA-Z.]+Exception.*)""")

  def toPrettyThrowable(throwable: Throwable): Throwable =
    throwable match
      case pekko.http.scaladsl.model.EntityStreamException(ErrorInfo(summary, _)) =>
        if summary.contains("connection was closed unexpectedly") then
          SimplifiedPekkoHttpException("Connection was closed unexpectedly", throwable)
        else
          SimplifiedPekkoHttpException(summary, throwable)

      case t if t.getClass.getName.endsWith("UnexpectedConnectionClosureException") =>
        logger.trace(s"toPrettyThrowable ${throwable.toStringWithCauses}")
        SimplifiedPekkoHttpException("Connection was closed unexpectedly", throwable)

      case t: pekko.stream.StreamTcpException =>
        t.getMessage match
          case PekkoTcpCommandRegex(command, host_) =>
            val host = host_.replace("/<unresolved>", "")
            val prefix = s"TCP $command $host"
            t.getCause match
              case t: java.net.SocketException =>
                logger.trace(s"toPrettyThrowable ${throwable.toStringWithCauses}")
                SimplifiedPekkoHttpException(prefix + ": " + t.getMessage, throwable)
              case t: java.net.UnknownHostException =>
                logger.trace(s"toPrettyThrowable ${throwable.toStringWithCauses}")
                SimplifiedPekkoHttpException(prefix + ": " + t.toString stripPrefix "java.net.",
                  throwable)
              case _ => throwable
          case _ => throwable

      case _ => throwable

  final class HttpException private[http](
    method: HttpMethod,
    val uri: Uri,
    httpResponse: HttpResponse,
    val dataAsString: String)
  extends HttpClient.HttpException, NoStackTrace:
    def statusInt: Int = status.intValue

    // Don't publish httpResponse because its entity stream has already been consumed for dataAsString
    def status: StatusCode = httpResponse.status

    def header[A <: HttpHeader: ClassTag]: Option[A] = httpResponse.header3[A]

    override def toString = getMessage

    override def getMessage =
      s"$prefixString => ${problem getOrElse shortDataString}"

    private def prefixString =
      s"HTTP ${httpResponse.status}: ${method.value} $uri"

    private def shortDataString = dataAsString.truncateWithEllipsis(10000)

    lazy val problem: Option[Problem] =
      (httpResponse.entity.contentType == `application/json`).thenMaybe:
        io.circe.parser.decode[Problem](dataAsString) match
          case Left(error) =>
            logger.debug(s"$uri: $prefixString, Problem cannot be parsed: $error - $dataAsString")
            None
          case Right(o) => Some(o)

  def hasRelevantStackTrace(throwable: Throwable | Null): Boolean =
    throwable != null && throwable.getStackTrace.nonEmpty &&
      throwable.match
        case _: pekko.stream.StreamTcpException => false
        case _: java.net.SocketException => false
        case _ => true

  def warnIdleTimeout: PartialFunction[Throwable, Stream[IO, Nothing]] =
    case e: IdleTimeoutException =>
      Stream.exec(IO:
        logger.warn(e.problem.toString))


  final class IdleTimeoutException private[PekkoHttpClient](uri: Uri, duration: FiniteDuration)
  extends ProblemException(HttpIdleTimeoutProblem(uri, duration)), NoStackTrace

  final class HttpIdleTimeoutProblem private[PekkoHttpClient](uri: Uri, duration: FiniteDuration)
  extends Problem.Coded:
    def arguments = Map2(
      "duration", duration.pretty,
      "uri", uri.toString)

  object HttpIdleTimeoutProblem extends Problem.Coded.Companion


  private final class SimplifiedPekkoHttpException(message: String, cause: Throwable)
  extends RuntimeException(message, cause), NoStackTrace:
    override def toString = getMessage: String
