package js7.common.pekkohttp

import com.typesafe.config.Config
import io.circe.parser.parse as parseJson
import java.lang.System.nanoTime
import java.util.Locale
import js7.base.auth.SessionToken
import js7.base.configutils.Configs.*
import js7.base.log.Logger.syntax.*
import js7.base.log.{CorrelId, LogLevel, Logger}
import js7.base.metering.CallMeter
import js7.base.problem.Problem
import js7.base.time.ScalaTime.*
import js7.base.time.Stopwatch.bytesPerSecondString
import js7.base.utils.Atomic.extensions.*
import js7.base.utils.ByteUnits.toKBGB
import js7.base.utils.ScalaUtils.syntax.*
import js7.common.http.HttpMXBean
import js7.common.http.PekkoHttpClient.{`x-js7-correlation-id`, `x-js7-request-id`, `x-js7-session`}
import js7.common.pekkohttp.WebLogDirectives.*
import js7.common.pekkohttp.web.auth.CSRF.forbidCSRF
import js7.common.pekkoutils.PekkoForExplicitNulls.header3
import org.apache.pekko.actor.ActorSystem
import org.apache.pekko.http.scaladsl.model.ContentTypes.{`application/json`, `text/plain(UTF-8)`}
import org.apache.pekko.http.scaladsl.model.headers.{Referer, `User-Agent`}
import org.apache.pekko.http.scaladsl.model.{AttributeKey, AttributeKeys, HttpEntity, HttpHeader, HttpMessage, HttpRequest, HttpResponse, StatusCode}
import org.apache.pekko.http.scaladsl.server.Directives.*
import org.apache.pekko.http.scaladsl.server.{Directive0, Route}
import scala.concurrent.duration.*
import scala.concurrent.duration.Deadline.now
import scala.reflect.ClassTag

/**
  * @author Joacim Zschimmer
  */
trait WebLogDirectives extends ExceptionHandling:
  protected def config: Config
  protected def actorSystem: ActorSystem

  private lazy val logLevel = LogLevel(config.getString("js7.web.server.log.level"))
  private lazy val errorLogLevel = LogLevel(config.getString("js7.web.server.log.error-level"))
  private lazy val internalServerErrorLevel = LogLevel(config.getString("js7.web.server.log.500-level"))
  private lazy val logRequest = actorSystem.settings.config.getBoolean("js7.web.server.log.request")
  private lazy val logResponse = actorSystem.settings.config.getBoolean("js7.web.server.log.response")

  protected final def mainRoute: Directive0 =
    meterRequest &
      (decodeRequest & encodeResponse) & // Before webLog to allow simple access to HttpEntity.Strict
      webLog &
      forbidCSRF

  private def meterRequest: Directive0 =
    mapInnerRoute: inner =>
      var t = 0L
      mapRequest: request =>
        HttpMXBean.Bean.serverRequestActiveCount += 1
        HttpMXBean.Bean.serverRequestTotal += 1
        // TODO Count streamed bytes, in and out
        t = System.nanoTime()
        request
      .apply:
        mapRouteResult: result =>
          requestCallMeter.addNanos(System.nanoTime() - t)
          HttpMXBean.Bean.serverRequestActiveCount -= 1
          result
        .apply:
          inner

  protected final def webLog: Directive0 =
    mapInnerRoute: inner =>
      extractRequest: request =>
        val correlId = getCorrelId(request)
        setCorrelIdAttribute(correlId):
          webLogOnly(request, correlId):
            seal:
              inner

  private def getCorrelId(request: HttpRequest): CorrelId =
    if !CorrelId.isEnabled then
      CorrelId.empty
    else
      //request.header[`x-js7-correlation-id`].fold(CorrelId.generate())(_.correlId)
      request.headers
        .find(_.is(`x-js7-correlation-id`.lowercaseName))
        .flatMap(h => CorrelId.checked(h.value).toOption)
        .getOrElse(CorrelId.generate())

  private def setCorrelIdAttribute(correlId: CorrelId)(route: Route): Route =
    if !CorrelId.isEnabled then
      route
    else
      mapRequest(_.addAttribute(CorrelIdAttributeKey, correlId)):
        route

  private def webLogOnly(request: HttpRequest, correlId: CorrelId): Directive0 =
    if !logRequest && !logResponse then
      pass
    else
      if logResponse then
        mapRequest(meterRequestTime(_, correlId, nanoTime)).tflatMap: _ =>
          val start = nanoTime
          mapResponse: response =>
            log(request, Some(response), correlId, statusToLogLevel(response.status), nanoTime - start)
            meterResponseTime(request, response, correlId, start)
      else
        pass
    end if

  private def meterRequestTime(request: HttpRequest, correlId: CorrelId, start: Long): HttpRequest =
    request.entity match
      case entity: HttpEntity.Strict =>
        HttpMXBean.Bean.serverReceivedByteTotal += entity.data.size
        if logRequest || webLogger.underlying.isTraceEnabled then
          log(request, None, correlId, if logRequest then logLevel else LogLevel.Trace, nanos = 0)
        request

      case entity: HttpEntity.Chunked =>
        logStream(request, request, entity, correlId, logLevel, start)

      case _ =>
        if logRequest || webLogger.underlying.isTraceEnabled then
          log(request, None, correlId, if logRequest then logLevel else LogLevel.Trace, nanos = 0)
        request

  private def meterResponseTime(request: HttpRequest, response: HttpResponse, correlId: CorrelId, start: Long)
  : HttpResponse =
    response.entity match
      case entity: HttpEntity.Strict =>
        HttpMXBean.Bean.serverSentByteTotal += entity.data.size
        response

      case entity: HttpEntity.Chunked =>
        logStream(request, response, entity, correlId, logLevel, start)

      case _ => response

  private def logStream[A <: HttpMessage](
    request: HttpRequest, message: A, entity: HttpEntity.Chunked,
    correlId: CorrelId, logLevel: LogLevel, start: Long)
  : message.Self =
    val since = now
    var chunkCount = 0L
    var byteCount = 0L
    message.withEntity:
      entity.copy(
        chunks =
          entity.chunks.wireTap: part =>
            chunkCount += 1
            byteCount += part.data.size
            message match
              case _: HttpRequest => HttpMXBean.Bean.serverReceivedByteTotal += part.data.size
              case _: HttpResponse => HttpMXBean.Bean.serverSentByteTotal += part.data.size
          .watchTermination(): (mat, future) =>
            future.onComplete { tried =>
              // Runs asynchronous in background
              log(request,
                (message: HttpMessage).ifSubtype[HttpResponse],
                correlId, logLevel, nanoTime - start,
                streamSuffix = // Timing of the stream, after response header has been sent
                  chunkCount.toString + " chunks, " +
                    bytesPerSecondString(since.elapsed, byteCount) +
                    tried.fold(t => " " + t.toStringWithCauses, _ => ""))
            }(using ioRuntime.compute)
            mat)

  private def log(request: HttpRequest, response: Option[HttpResponse],
    correlId: CorrelId, logLevel: LogLevel, nanos: Long, streamSuffix: String = "")
  : Unit =
    correlId.bind:
      webLogger.log(
        logLevel,
        requestResponseToLine(request, response, nanos, streamSuffix))

  private def statusToLogLevel(statusCode: StatusCode): LogLevel =
    statusCode match
      case status if status.intValue < 400 => logLevel
      case status if status.intValue == 500 => internalServerErrorLevel
      case _ => errorLogLevel

  private def requestResponseToLine(request: HttpRequest, maybeResponse: Option[HttpResponse],
    nanos: Long, streamSuffix: String)
  : String =
    val sb = new StringBuilder(256)

    def appendHeader[A <: HttpHeader: ClassTag](): Unit =
      request.header3[A] match
        case None => sb.append(" -")
        case Some(h) => appendQuotedString(sb, h.value)

    maybeResponse match
      case None =>
        if streamSuffix.isEmpty then
          sb.append("-->")
        else
          sb.append("|->") // Request stream ended
      case Some(response) =>
        if streamSuffix.isEmpty then
          sb.append(response.status.intValue)
        else
          sb.append("<-|") // Response stream ended
    sb.append(' ')
    // Let SessionToken and request number look like in PekkoHttpClient
    sb.append(request.headers
      .collectFirst:
        case `x-js7-session`(secret) => SessionToken.stringToShort(secret)
      .getOrElse("-"))
    sb.append(' ')
    sb.append(request.headers.collectFirst { case `x-js7-request-id`(id) => id } getOrElse "#")
    sb.append(' ')
    sb.append(request.attribute(AttributeKeys.remoteAddress).flatMap(_.toIP)
      .fold("-")(_.ip.getHostAddress))
    sb.append(' ')
    if streamSuffix.nonEmpty then
      sb.append(request.method.value.toLowerCase(Locale.ROOT))
    else
      sb.append(request.method.value)
    sb.append(' ')
    sb.append(request.uri)
    maybeResponse match
      case None =>
        appendHeader[Referer]()
        appendHeader[`User-Agent`]()
        if streamSuffix.nonEmpty then
          sb.append(" · ")
          sb.append(streamSuffix)

      case Some(response) =>
        if response.status.isFailure then
          response.entity match  // Try to extract error message
            case entity @ HttpEntity.Strict(`text/plain(UTF-8)`, _) =>
              val string = entity.data.utf8String
              val truncated = string.take(1001).dropLastWhile(_ == '\n').map(c => if c.isControl then '·' else c)
              appendQuotedString(sb, truncated + ((truncated.length < string.length) ?? "..."))

            case entity @ HttpEntity.Strict(`application/json`, _) =>
              parseJson(entity.data.utf8String).flatMap(_.as[Problem]) match
                case Left(_) => appendQuotedString(sb, response.status.reason)
                case Right(problem) => appendQuotedString(sb, problem.toString)

            case _ =>
              appendQuotedString(sb, response.status.reason)
        else response.entity match
          case entity: HttpEntity.Strict =>
            sb.append(" · ")
            sb.append(toKBGB(entity.data.length))
            sb.append(' ')
            sb.append(nanos.nanoseconds.pretty)
          case _ =>
            if streamSuffix.isEmpty then
              sb.append(" · STREAM... ")
              sb.append(nanos.nanoseconds.pretty)
            else
              sb.append(" · ")
              sb.append(streamSuffix)
    sb.toString


object WebLogDirectives:
  private val webLogger = Logger("js7.web.log")

  val CorrelIdAttributeKey: AttributeKey[CorrelId] =
    AttributeKey[CorrelId]("CorrelId")

  val TestConfig = config"""
    js7.web.server.log.level = debug
    js7.web.server.log.error-level = debug
    js7.web.server.log.500-level = info
    js7.web.server.log.duration = off
    js7.web.server.verbose-error-messages = true
    js7.web.server.shutdown-timeout = 10s
    js7.web.server.shutdown-delay = 500ms
    """

  // Prometheus name: http_request_duration_seconds
  private val requestCallMeter = CallMeter("web server request")

  private def appendQuotedString(sb: StringBuilder, string: String) =
    sb.ensureCapacity(3 + string.length)
    sb.append(" \"")
    if !string.contains('"') then
      sb.append(string)
    else
      string foreach:
        case '"' => sb.append('\\').append('"')
        case ch => sb.append(ch)
    sb.append('"')
