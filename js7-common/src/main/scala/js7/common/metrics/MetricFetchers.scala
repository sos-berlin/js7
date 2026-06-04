package js7.common.metrics

import cats.effect.IO
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
import js7.common.metrics.MetricFetchers.*
import js7.common.metrics.MetricsProvider.toPrometheuesErrorLines
import js7.common.pekkohttp.PekkoHttpServerUtils.completeWithIOStream
import js7.common.pekkoutils.ByteStrings.syntax.*
import js7.data.node.Js7ServerId
import org.apache.pekko.actor.ActorSystem
import org.apache.pekko.http.scaladsl.model.{ContentType, HttpEntity}
import org.apache.pekko.http.scaladsl.server.Route
import org.apache.pekko.util.ByteString

final case class MetricFetchers(
  js7ServerId: Option[Js7ServerId],
  httpChunkSize: Int,
  conf: CommonConfiguration)
  (using ActorSystem):

  private val uriToHttp = AsyncMap[Js7ServerId, Allocated[IO, StandardHttpClient]]

  // TODO Still unused!
  def release: IO[Unit] =
    uriToHttp.removeAll.flatMap:
      _.values.toVector.parTraverseVoid:
        _.release

  def remoteMetricFetcher(
    serverId: Js7ServerId,
    admission: Admission,
    uriPath: String,
    deep: Boolean,
    label: String)
  : MetricFetcher =
    val stream = fs2.Stream.force:
      uriToHttp
        .getOrElseUpdate(
          serverId,
          StandardHttpClient.resource(admission, uriPath = uriPath, conf.httpsConfig, label = label)
            .toAllocated)
        .map: allocated =>
          Right(allocated.allocatedThing)
        .flatMapT: httpClient =>
          HttpClient.liftProblem:
            httpClient.loginAndRetryIfSessionLost:
              import httpClient.implicitSessionToken
              httpClient.get_[HttpEntity](
                admission.uri / s"metrics${deep ?? "?deep=true"}",
                MetricsProvider.PrometheusRequestHeaders)
        .map:
          case Left(problem) =>
            logger.debug(s"💥 Error when accessing $serverId: $problem")
            val problemString = problem.toString.truncateWithEllipsis(1000, firstLineOnly = true)
            fs2.Stream.emit(ByteString(toPrometheuesErrorLines(s"$serverId: $problemString")))
          case Right(entity) =>
            entity.dataBytes.asFs2Stream()
    MetricFetcher(serverId, stream)

  /** Complete the request through concurrent execution of MetricFetchers. */
  def completeMetricFetchers(contentType: ContentType, metricFetchers: Seq[MetricFetcher])
  : IO[Route] =
    val (serverIds, streams) = metricFetchers.map(_.toPair).unzip
    releaseUnknownHttpClients(isKnown = serverIds.toSet).productR:
      completeWithIOStream(contentType):
        MetricsProvider.mergeMetricStreams(streams.toList)
          .map(_.toChunk).unchunks[Byte].chunkN(httpChunkSize)
          .map(_.toByteString)

  private def releaseUnknownHttpClients(isKnown: Js7ServerId => Boolean): IO[Unit] =
    uriToHttp.toMap.map:
      _.toVector.parTraverseVoid: (serverId, allocated) =>
        IO.whenA(isKnown(serverId)):
          uriToHttp.remove(serverId) *>
            allocated.release


object MetricFetchers:
  private val logger = Logger[this.type]
  private val LF = ByteString("\n")
