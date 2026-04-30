package js7.common.metrics

import java.nio.file.Path
import js7.base.log.Logger
import js7.base.system.JavaServiceProviders.findJavaService
import js7.base.utils.ScalaUtils.syntax.{RichString, RichThrowable}
import js7.common.pekkoutils.ByteStrings.syntax.*
import js7.data.node.Js7ServerId
import org.apache.pekko.http.scaladsl.model.HttpCharsets.`UTF-8`
import org.apache.pekko.http.scaladsl.model.MediaTypes.{`application/json`, `text/plain`}
import org.apache.pekko.http.scaladsl.model.headers.{Accept, `Accept-Charset`}
import org.apache.pekko.http.scaladsl.model.{ContentType, HttpHeader, MediaType}
import org.apache.pekko.util.ByteString
import scala.util.control.NonFatal

object MetricsProvider:

  type ToMetricsStream = () => fs2.Stream[fs2.Pure, ByteString]
  type ToMetricsByteString = () => ByteString

  private val logger = Logger[this.type]

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

  def toMetricsByteString(
    ownJs7ServerId: Js7ServerId,
    configDirectory: Option[Path] = None)
  : ToMetricsByteString =
    val toStream = toMetricsStream(ownJs7ServerId, configDirectory)
    () => toStream().compile.foldMonoid

  def toMetricsStream(ownJs7ServerId: Js7ServerId, configDirectory: Option[Path] = None)
  : ToMetricsStream =
    val addAttribute = s"js7Server=\"${toPrometheusString(ownJs7ServerId.toString)}\""
    () =>
      javaService match
        case None =>
          fs2.Stream.emit:
            ByteString(s"# $ownJs7ServerId: No MetricsJavaService installed \n")

        case Some(svc) =>
          try
            svc.metricsLines(configDirectory)(addAttribute = addAttribute)
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
