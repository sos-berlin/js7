package js7.service.prometheus

import io.prometheus.jmx.JmxCollector
import io.prometheus.metrics.expositionformats.PrometheusTextFormatWriter
import io.prometheus.metrics.model.registry.PrometheusRegistry
import java.io.IOException
import java.lang.management.ManagementFactory
import java.nio.file.{NoSuchFileException, Path}
import js7.base.data.{ByteSeqOutputStream, ByteSequence}
import js7.base.io.JavaResource
import js7.base.io.file.FileUtils.syntax.*
import js7.base.log.Logger
import js7.base.log.Logger.syntax.*
import js7.base.metering.CallMeter
import js7.base.utils.ScalaUtils.syntax.foreachWithBracket
import js7.service.prometheus.PrometheusJmxAdapter.*
import scala.jdk.CollectionConverters.*

private[prometheus] final class PrometheusJmxAdapter(configDir: Option[Path] = None):

  logger.info("Using Prometheus monitoring")

  private val registry = new PrometheusRegistry
  private val writer = PrometheusTextFormatWriter.builder().build()
  private var lastSize = 128 * 1024
  private var contentTypeWarned = false

  new JmxCollector(
    configDir.flatMap: configDir =>
      val file = configDir / "prometheus-jmx.yaml"
      try
        Some(file.contentString)
      catch
        case _: NoSuchFileException => None
        case e: IOException =>
          logger.error(s"$file: $e • Using default configuration")
          None
    .getOrElse:
      DefaultYamlResource.asUTF8String
  ).register(registry)

  def metrics[ByteSeq: ByteSequence](): ByteSeq =
    logger.traceCall:
      meter:
        val outputStream = new ByteSeqOutputStream(lastSize + lastSize / 5)
        writer.write(outputStream, registry.scrape())
        lastSize = outputStream.size()

        val contentType = writer.getContentType
        if contentType != "text/plain; version=0.0.4; charset=utf-8" then
          if !contentTypeWarned then
            logger.warn(s"Prometheus JMX adapter returned an unexpected content type: $contentType")
            contentTypeWarned = true

        outputStream.unsafeByteSeq[ByteSeq]


private[prometheus] object PrometheusJmxAdapter:
  private val logger = Logger[this.type]
  private val meter = CallMeter("PrometheusJmxAdapter")
  private val DefaultYamlResource = JavaResource("js7/service/prometheus/prometheus-jmx.yaml")

  logger.whenTraceEnabled:
    logger.trace("Available JMX MBeans:")
    ManagementFactory.getPlatformMBeanServer.queryNames(null, null).asScala.toArray
      .map(_.getCanonicalName).sorted.foreachWithBracket(): (o, br) =>
        logger.trace(s"$br$o")
