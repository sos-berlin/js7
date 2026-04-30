package js7.service.prometheus

import cats.effect.IO
import java.nio.file.Path
import js7.common.metrics.MetricsJavaService
import org.apache.pekko.util.ByteString

final class PrometheusMetricsJavaService extends MetricsJavaService:

  def metricsLines(configDirectory: Option[Path])
  : (addAttribute: String) => fs2.Stream[IO, ByteString] =
    val adapter = new PrometheusJmxAdapter(configDirectory)
    addAttribute => adapter.metricsLines(addAttribute = addAttribute)
