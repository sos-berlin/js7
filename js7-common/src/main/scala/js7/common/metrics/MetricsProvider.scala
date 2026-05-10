package js7.common.metrics

import cats.effect.IO
import cats.syntax.foldable.*
import java.nio.file.Path
import java.util.UUID.randomUUID
import js7.base.fs2utils.ByteChunksLineSplitter.byteChunksToLines
import js7.base.log.Logger
import js7.base.system.JavaServiceProviders.findJavaService
import js7.base.utils.ScalaUtils.syntax.RichString
import js7.common.pekkoutils.ByteStrings.syntax.*
import js7.data.node.{Js7ServerGroupId, Js7ServerId}
import org.apache.pekko.http.scaladsl.model.HttpCharsets.`UTF-8`
import org.apache.pekko.http.scaladsl.model.MediaTypes.{`application/json`, `text/plain`}
import org.apache.pekko.http.scaladsl.model.headers.{Accept, `Accept-Charset`}
import org.apache.pekko.http.scaladsl.model.{ContentType, HttpHeader, MediaType}
import org.apache.pekko.util.ByteString
import scala.util.control.NonFatal

object MetricsProvider:

  private val logger = Logger[this.type]
  private val EndMarker = ByteString(s"# ${randomUUID()}\n")

  /** The 'accept' header of Prometheus 1_4_3. */
  val PrometheusAcceptHeaderValue: String =
    "application/openmetrics-text; escaping=allow-utf-8; version=1.0.0;q=0.6, " +
      "application/openmetrics-text; version=0.0.1;q=0.5, " +
      "text/plain; escaping=allow-utf-8; version=1.0.0;q=0.4, " +
      "text/plain; version=0.0.4;q=0.3," +
      "*/*;q=0.2)"

  // Prometheus request example:
  // GET /metrics HTTP/1.1
  // User-Agent: Prometheus/3.11.2
  // Accept: $PrometheusAcceptHeaderValue (see above)
  // Accept-Encoding: gzip
  // X-Prometheus-Scrape-Timeout-Seconds: 5

  /** This seems to be what Prometheus is accepting and the /metrics web service is returning. */
  private val `text/plain;version=0.0.4`: ContentType =
    MediaType.customWithFixedCharset(
      "text", "plain", `UTF-8`, params = Map("version" -> "0.0.4")
    ).toContentType

  val PrometheusContentType: ContentType =
    `text/plain;version=0.0.4`

  //private val `application/openmetrics-text 0.0.4`: ContentType =
  //  MediaType.customWithFixedCharset(
  //      "application", "openmetrics-text", `UTF-8`, params = Map(
  //        "version" -> "0.0.4",
  //        "escaping" -> "allow-utf-8" /*Use \ as escape character for \, " and \n*/))
  //    .toContentType

  private val mediaTypes: Set[MediaType] =
    Set(
      //`application/openmetrics-text 0.0.4`.mediaType,
      `text/plain;version=0.0.4`.mediaType)

  //private val acceptedContentTypes: List[ContentType] =
  //  List(
  //    `text/plain;version=0.0.4`,
  //    //`application/openmetrics-text 0.0.4`,
  //    `application/json`.toContentType /*Problem is JSON-encoded*/ ,
  //    `text/plain(UTF-8)`)

  val PrometheusAcceptHeader: Accept =
    Accept(
      `text/plain;version=0.0.4`.mediaType.withQValue(1.0),
      //`application/openmetrics-text 0.0.4`.mediaType.withQValue(1.0),
      `application/json` /*Problem is JSON-encoded*/.withQValue(0.9),
      `text/plain`.withQValue(0.1))

  val PrometheusRequestHeaders: List[HttpHeader] =
    List(
      `Accept-Charset`(`UTF-8`),
      PrometheusAcceptHeader)

  private lazy val javaService: Option[MetricsJavaService] =
    findJavaService[MetricsJavaService]

  def toMetricsStream(configDirectory: Option[Path] = None)
  : (groupAndServerId: (Js7ServerGroupId, Js7ServerId)) => fs2.Stream[IO, ByteString] =
    javaService match
      case None =>
        (groupId, serverId) =>
          fs2.Stream.emit:
            ByteString(s"# $serverId ($groupId): No MetricsJavaService installed \n")

      case Some(svc) =>
        val metricsLines = svc.toMetricLineStream(configDirectory)
        (groupId: Js7ServerGroupId, js7ServerId: Js7ServerId) =>
          val sb = new StringBuilder(64)
          sb.append("js7ServerId=\"")
          sb.append(toPrometheusString(js7ServerId.toString))
          sb.append("\"")
          sb.append(",js7ServerGroupId=\"")
          sb.append(toPrometheusString(groupId.toString))
          sb.append("\"")
          try
            metricsLines(addAttribute = sb.toString)
          catch case NonFatal(t) =>
            logger.error(s"toMetricsStream: ${t.toString}", t)
            fs2.Stream.emit(ByteString(toPrometheuesErrorLines(t.toString)))

  def toPrometheuesErrorLines(message: String): String =
    s"\n# ERROR: ${message.truncateWithEllipsis(1000, firstLineOnly = true)}\n\n"

  private def toPrometheusString(string: String): String =
    string
      .replace("\"", "\\\"")
      .replace("\n", "\\n")
      .replace("\\", "\\\\")

  /** Merge several Prometheus metric streams asynchronously. */
  def mergeMetricStreams(streams: Seq[fs2.Stream[IO, ByteString]])
  : fs2.Stream[IO, ByteString] =
    locally:
      if streams.sizeIs <= 1 then
        streams.combineAll
      else
        // Deliver measurement of all sources concurrently merged
        streams.toList.map:
          _.through(separateMeasurements) // Merge only whole measurements
        .parJoinUnbounded

  /** A stream of ByteStrings where each contains a single measurement.
    *
    * The incoming stream may be any chunks.
    *
    * The outcoming stream consists of a ByteString for each measurement.
    * A measurement consists of comment lines followed by data lines.
    */
  private[metrics] def separateMeasurements[F[_]]: fs2.Pipe[F, ByteString, ByteString] =
    _.through:
      byteChunksToLines(breakLinesLongerThan = None)
    .append(fs2.Stream.emit:
      EndMarker /*to force emission of last collected measurement, will be dropped at end*/)
    .mapAccumulate((ByteString.empty, false)):
      case ((measurement, lastWasData), line) =>
        if line.startsWith("#") then
          if lastWasData then // First comment after data: flush
            ((line, false), measurement)
          else
            ((measurement ++ line, false), ByteString.empty)
        else
          ((measurement ++ line, true), ByteString.empty)
    .map(_._2)
    .filter: byteString =>
      byteString.nonEmpty && byteString != EndMarker
