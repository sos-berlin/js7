package js7.common.metrics

import cats.effect.IO
import cats.syntax.foldable.*
import cats.syntax.parallel.*
import js7.base.auth.Admission
import js7.base.data.ByteSequence.ops.*
import js7.base.fs2utils.Fs2ChunkByteSequence.implicitByteSequence
import js7.base.log.Logger
import js7.base.monixutils.AsyncMap
import js7.base.utils.Allocated
import js7.base.utils.CatsUtils.syntax.*
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.web.HttpClient
import js7.common.configuration.CommonConfiguration
import js7.common.http.StandardHttpClient
import js7.common.http.StreamingSupport.asFs2Stream
import js7.common.metrics.MetricsProvider.{splitMeasurements, toPrometheuesErrorLines}
import js7.common.metrics.RemoteMetricsRoute.*
import js7.common.pekkohttp.PekkoHttpServerUtils.completeWithIOStream
import js7.common.pekkoutils.ByteStrings.syntax.*
import js7.data.node.Js7ServerId
import org.apache.pekko.actor.ActorSystem
import org.apache.pekko.http.scaladsl.model.{ContentType, HttpEntity}
import org.apache.pekko.http.scaladsl.server.Route
import org.apache.pekko.util.ByteString

trait RemoteMetricsRoute extends MetricsRoute:

  protected def commonConf: CommonConfiguration
  protected def actorSystem: ActorSystem
  private given ActorSystem = actorSystem

  private val uriToHttp = AsyncMap[Js7ServerId, Allocated[IO, StandardHttpClient]]

  // TODO Still unused!
  def release: IO[Unit] =
    uriToHttp.toMap.flatMap:
      _.values.toVector.parTraverseVoid:
        _.release

  protected final def remoteMetricFetcher(
    serverId: Js7ServerId,
    admission: Admission,
    uriPath: String,
    onlyThisServer: Boolean,
    label: String)
  : MetricFetcher =
    MetricFetcher(
      serverId,
      stream =
        fs2.Stream.force:
          uriToHttp
            .getOrElseUpdate(
              serverId,
              StandardHttpClient.resource(
                admission, uriPath = uriPath, commonConf.httpsConfig, label = label
              ).toAllocated)
            .map: allocated =>
              Right(allocated.allocatedThing)
            .flatMapT: httpClient =>
              HttpClient.liftProblem:
                httpClient.loginAndRetryIfSessionLost:
                  import httpClient.implicitSessionToken
                  httpClient.get_[HttpEntity](
                    admission.uri / s"metrics${onlyThisServer ?? "?onlyThisServer=true"}",
                    MetricsProvider.PrometheusRequestHeaders)
            .map:
              case Left(problem) =>
                logger.debug(s"💥 Error when accessing $serverId: $problem")
                val problemString = problem.toString.truncateWithEllipsis(1000, firstLineOnly = true)
                fs2.Stream.emit(ByteString(toPrometheuesErrorLines("$serverId: $problemString")))
              case Right(entity) =>
                entity.dataBytes.asFs2Stream())
  end remoteMetricFetcher

  /** Complete the request through concurrent execution of MetricFetchers. */
  protected final def completeMetricFetchers(
    contentType: ContentType,
    metricFetchers: Seq[MetricFetcher])
  : IO[Route] =
    val (serverIds, streams) = metricFetchers.map(_.toPair).unzip
    releaseUnknownHttpClients(isKnown = serverIds.toSet).productR:
      completeWithIOStream(contentType):
        val allStreams = toMetricsStream().covary[IO] :: streams.toList
        locally:
          if streams.sizeIs <= 1 then
            allStreams
              .combineAll
          else
            // Deliver measurement of all sources concurrently merged
            allStreams.map:
              _.through(splitMeasurements)
            .parJoinUnbounded
        .map(_.toChunk).unchunks
        .chunkN(httpChunkSize)
        .map(_.toByteString)

  private def releaseUnknownHttpClients(isKnown: Js7ServerId => Boolean): IO[Unit] =
    uriToHttp.toMap.map:
      _.toVector.parTraverseVoid: (serverId, allocated) =>
        IO.whenA(isKnown(serverId)):
          uriToHttp.remove(serverId) *>
            allocated.release


object RemoteMetricsRoute:
  private val logger = Logger[this.type]
  private val LF = ByteString("\n")

  /** Wrapped IO that fetches Prometheus metrics for a Js7ServerId.
    */
  final case class MetricFetcher(js7ServerId: Js7ServerId, stream: fs2.Stream[IO, ByteString]):
    def toPair = (js7ServerId, stream)
