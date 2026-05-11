package js7.common.metrics

import cats.effect.IO
import js7.base.log.Logger
import js7.base.monixutils.AsyncMap
import js7.base.utils.Allocated
import js7.common.configuration.CommonConfiguration
import js7.common.http.StandardHttpClient
import js7.data.node.Js7ServerId
import org.apache.pekko.actor.ActorSystem

trait RemoteMetricsRoute extends MetricsRoute:

  protected def commonConf: CommonConfiguration
  protected def actorSystem: ActorSystem
  private given ActorSystem = actorSystem

  private val uriToHttp = AsyncMap[Js7ServerId, Allocated[IO, StandardHttpClient]]

  private lazy val metricFetchers = MetricFetchers(
    groupAndServerId.map(_.serverId),
    httpChunkSize = httpChunkSize, commonConf)

  // TODO Still unused!
  final def release: IO[Unit] =
    metricFetchers.release

  export metricFetchers.{remoteMetricFetcher, completeMetricFetchers}


object RemoteMetricsRoute:
  private val logger = Logger[this.type]
