package js7.cluster.web

import cats.effect.IO
import cats.effect.unsafe.IORuntime
import fs2.Stream
import js7.base.auth.UserId
import js7.base.fs2utils.StreamExtensions.interruptWhenF
import js7.base.log.Logger
import js7.base.problem.Checked
import js7.base.utils.ScalaUtils.syntax.*
import js7.cluster.web.ClusterWatchRequestRoute.*
import js7.common.http.JsonStreamingSupport.`application/x-ndjson`
import js7.common.pekkohttp.PekkoHttpServerUtils
import js7.common.pekkohttp.PekkoHttpServerUtils.{accept, completeWithStream, encodeAndHeartbeat}
import js7.common.pekkohttp.StandardMarshallers.*
import js7.common.pekkohttp.web.session.RouteProvider
import js7.data.cluster.{ClusterState, ClusterWatchRequest}
import js7.data.event.Stamped
import js7.data.node.NodeId
import js7.journal.watch.EventWatch
import org.apache.pekko.http.scaladsl.server.Directives.*
import org.apache.pekko.http.scaladsl.server.Route
import scala.concurrent.duration.FiniteDuration

trait ClusterWatchRequestRoute extends RouteProvider:

  private given IORuntime = ioRuntime

  protected def checkedClusterState: IO[Checked[Stamped[ClusterState]]]
  protected def clusterWatchRequestStream: fs2.Stream[IO, ClusterWatchRequest]
  protected def eventWatch: EventWatch
  protected def nodeId: NodeId

  protected final def clusterWatchMessageRoute(userId: UserId): Route =
    Route.seal:
      accept(`application/x-ndjson`):
        extractRequest: request =>
          parameter("keepAlive".as[FiniteDuration]): keepAlive =>
            completeWithStream(`application/x-ndjson`):
              clusterWatchRequestStream
                .handleErrorWith: throwable =>
                  // The streaming event web service doesn't have an error channel,
                  // so we simply log the error and end the stream
                  logger.warn(throwable.toStringWithCauses)
                  if throwable.getStackTrace.nonEmpty then
                    logger.debug(throwable.toStringWithCauses, throwable)
                  Stream.empty
                .through:
                  encodeAndHeartbeat(
                    httpChunkSize = httpChunkSize,
                    prefetch = prefetch,
                    keepAlive = keepAlive)
                .interruptWhenF(shutdownSignaled)


object ClusterWatchRequestRoute:
  private val logger = Logger[this.type]
