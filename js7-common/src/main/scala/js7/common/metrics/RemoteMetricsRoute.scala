package js7.common.metrics

import cats.effect.IO
import js7.base.log.Logger
import js7.base.monixutils.AsyncMap
import js7.base.utils.Allocated
import js7.common.configuration.CommonConfiguration
import js7.common.http.StandardHttpClient
import js7.data.node.Js7ServerId
import org.apache.pekko.actor.ActorSystem
import org.apache.pekko.http.scaladsl.model.ContentType
import org.apache.pekko.http.scaladsl.server.{Directives, Route}

trait RemoteMetricsRoute extends MetricsRoute:

  protected def commonConf: CommonConfiguration
  protected def actorSystem: ActorSystem
  private given ActorSystem = actorSystem

  protected def deepMetricsRoute(contentType: ContentType): Route

  private val uriToHttp = AsyncMap[Js7ServerId, Allocated[IO, StandardHttpClient]]

  private lazy val metricFetchers = MetricFetchers(
    groupAndServerId.map(_.serverId),
    httpChunkSize = httpChunkSize, commonConf)

  // TODO Still unused!
  final def release: IO[Unit] =
    metricFetchers.release

  /** /metrics web service according to Prometheus.
    * <p>
    * Prometheus expects a web service path "/metrics".
    *
    * @see https://prometheus.io/docs/concepts/data_model/#metric-names-and-labels
    * @see https://prometheus.io/docs/practices/naming/
    */
  override final lazy val metricsRoute: Route =
    wrapMetricsRoute: contentType =>
      import Directives.*
      parameter("deep" ? false):
        case false =>
          onlyThisServerMetricsRoute(contentType)
        case true =>
          deepMetricsRoute(contentType)

  export metricFetchers.{remoteMetricFetcher, completeMetricFetchers}


object RemoteMetricsRoute:
  private val logger = Logger[this.type]
