package js7.common.metrics

import cats.effect.IO
import cats.effect.unsafe.IORuntime
import com.typesafe.config.Config
import fs2.{Chunk, Pure}
import java.nio.charset.StandardCharsets.UTF_8
import js7.base.auth.ReadMetricsPermission
import js7.base.fs2utils.Fs2ChunkByteSequence.implicitByteSequence
import js7.base.utils.Atomic
import js7.base.utils.Atomic.extensions.*
import js7.common.configuration.CommonConfiguration
import js7.common.pekkohttp.PekkoHttpServerUtils.completeIO
import js7.common.pekkohttp.web.session.RouteProvider
import js7.common.pekkoutils.ByteStrings.syntax.*
import js7.data.node.Js7ServerId
import org.apache.pekko.http.scaladsl.model.ContentTypes.`text/plain(UTF-8)`
import org.apache.pekko.http.scaladsl.model.HttpCharsets.`UTF-8`
import org.apache.pekko.http.scaladsl.model.MediaTypes.{`application/json`, `text/plain`}
import org.apache.pekko.http.scaladsl.model.StatusCodes.TooManyRequests
import org.apache.pekko.http.scaladsl.model.headers.{Accept, `Accept-Charset`}
import org.apache.pekko.http.scaladsl.model.{ContentType, HttpEntity, HttpHeader, HttpResponse, MediaType}
import org.apache.pekko.http.scaladsl.server.Directives.*
import org.apache.pekko.http.scaladsl.server.Route
import org.apache.pekko.util.ByteString

trait MetricsRoute extends RouteProvider:

  protected def commonConf: CommonConfiguration
  protected def js7ServerId: Option[Js7ServerId]

  private val isLocked = Atomic(false)

  protected final def config: Config =
    commonConf.config

  private lazy val metricsProvider: () => fs2.Stream[Pure, Chunk[Byte]] =
    js7ServerId match
      case None =>
        () => fs2.Stream.emit(fs2.Chunk.array("# Unknown Js7ServerId\n".getBytes(UTF_8)))
      case Some(serverId) =>
        MetricsProvider.toMetricsProvider(serverId, Some(commonConf.configDirectory))

  protected final def metricsRawRoute(contentType: ContentType): Route =
    given IORuntime = ioRuntime
    completeIO:
      IO: // Execute each time again
        HttpResponse(entity = metricsHttpEntity(contentType))

  /** /metrics web service according to Prometheus.
    * <p>
    *   Prometheus expects a web service path "/metrics".
    *
    * @see https://prometheus.io/docs/concepts/data_model/#metric-names-and-labels
    * @see https://prometheus.io/docs/practices/naming/
    */
  protected final lazy val metricsRoute: Route =
    wrapMetricsRoute:
      metricsRawRoute

  protected final def wrapMetricsRoute(route: ContentType => Route): Route =
    authorized(ReadMetricsPermission):
      // Prometheus accepts text/plain
      (pathEnd & get /*& respondWithContentType(acceptedContentTypes)*/): /*contentType =>*/
        onlyOneRequest:
          route(`text/plain(UTF-8)`)

  private final def onlyOneRequest(route: Route): Route =
    if isLocked.getAndSet(true) then
      complete(TooManyRequests/*?*/, "Another request has not completed. Try again later.")
    else
      mapRouteResult(result =>
        isLocked := false
        result
      )(
        try
          route
        finally
          isLocked := false)

  def metricsHttpEntity(contentType: ContentType): HttpEntity.Strict =
    HttpEntity(contentType, metricsByteString())

  final def metricsByteString(): ByteString =
    js7ServerId match
      case None =>
        ByteString("# Js7ServerId is still unknown\n")
      case Some(serverId) =>
        val qServerId = serverId.toString.replace("\"", "\\\"").replace("\n", "\\n").replace("\\", "\\\\")
        metricsProvider().compile.foldMonoid.toByteString // Complete response is kept in memory !!!

  private def quoteMetricString(string: String): String =
    string.replace("\"", "\\\"").replace("\n", "\\n").replace("\\", "\\\\")


object MetricsRoute:

  /** This seems to be what the /metrics web service is returning. */
  private val `application/openmetrics-text 0.0.4`: ContentType =
    MediaType.customWithFixedCharset(
        "application", "openmetrics-text", `UTF-8`, params = Map(
          "version" -> "0.0.4",
          "escaping" -> "allow-utf-8" /*Use \ as escape character for \, " and \n*/))
      .toContentType

  // Prometheus request example:
  // GET /metrics HTTP/1.1
  // User-Agent: Prometheus/3.11.2
  // Accept:
  //   application/openmetrics-text; escaping=allow-utf-8; version=1.0.0;q=0.6,
  //   application/openmetrics-text; version=0.0.1;q=0.5,
  //   text/plain; escaping=allow-utf-8; version=1.0.0;q=0.4,
  //   text/plain; version=0.0.4;q=0.3, */*;q=0.2
  // Accept-Encoding: gzip
  // X-Prometheus-Scrape-Timeout-Seconds: 5

  private val mediaTypes: Set[MediaType] =
    Set(
      `application/openmetrics-text 0.0.4`.mediaType,
      `text/plain`)

  private val acceptedContentTypes: List[ContentType] =
    List(
      `application/openmetrics-text 0.0.4`,
      `application/json`.toContentType/*Problem is JSON-encoded*/,
      `text/plain(UTF-8)`)

  val requestHeaders: List[HttpHeader] =
    List(
      `Accept-Charset`(`UTF-8`),
      Accept(
        `application/openmetrics-text 0.0.4`.mediaType.withQValue(1.0),
        `application/json`/*Problem is JSON-encoded*/.withQValue(0.9),
        `text/plain`.withQValue(0.1)))
