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
import js7.common.metrics.MetricsProvider.PrometheusContentType
import js7.common.pekkohttp.PekkoHttpServerUtils.completeIO
import js7.common.pekkohttp.web.session.RouteProvider
import js7.common.pekkoutils.ByteStrings.syntax.*
import js7.data.node.Js7ServerId
import org.apache.pekko.http.scaladsl.model.StatusCodes.TooManyRequests
import org.apache.pekko.http.scaladsl.model.{ContentType, HttpEntity, HttpResponse}
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
          route(PrometheusContentType)

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
