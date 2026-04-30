package js7.common.metrics

import cats.effect.unsafe.IORuntime
import com.typesafe.config.Config
import js7.base.auth.ReadMetricsPermission
import js7.base.data.ByteSequence.ops.*
import js7.base.fs2utils.Fs2ChunkByteSequence.implicitByteSequence
import js7.base.utils.Atomic
import js7.base.utils.Atomic.extensions.*
import js7.common.configuration.CommonConfiguration
import js7.common.metrics.MetricsProvider.PrometheusContentType
import js7.common.pekkohttp.PekkoHttpServerUtils.completeWithStream
import js7.common.pekkohttp.web.session.RouteProvider
import js7.common.pekkoutils.ByteStrings.syntax.*
import js7.data.node.Js7ServerId
import org.apache.pekko.http.scaladsl.model.ContentType
import org.apache.pekko.http.scaladsl.model.StatusCodes.TooManyRequests
import org.apache.pekko.http.scaladsl.server.Directives.*
import org.apache.pekko.http.scaladsl.server.Route
import org.apache.pekko.util.ByteString

trait MetricsRoute extends RouteProvider:

  protected def commonConf: CommonConfiguration
  protected def js7ServerId: Option[Js7ServerId]

  private val isLocked = Atomic(false)

  protected final def config: Config =
    commonConf.config

  lazy val toMetricsStream: () => fs2.Stream[fs2.Pure, ByteString] =
    val toMetricsStream = MetricsProvider.toMetricsStream(Some(commonConf.configDirectory))
    val unknown = fs2.Stream.emit(ByteString("# Js7ServerId of this server is (still) unknown\n"))
    () =>
      js7ServerId match
        case None =>
          unknown
        case Some(serverId) =>
          toMetricsStream(serverId)

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

  protected final def metricsRawRoute(contentType: ContentType): Route =
    given IORuntime = ioRuntime
    completeWithStream(contentType):
      toMetricsStream()
        .map(_.toChunk).unchunks
        .chunkN(httpChunkSize)
        .map(_.toByteString)

