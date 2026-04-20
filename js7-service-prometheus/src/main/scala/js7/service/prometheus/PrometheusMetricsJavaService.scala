package js7.service.prometheus

import java.nio.file.Path
import js7.common.metrics.MetricsJavaService

final class PrometheusMetricsJavaService extends MetricsJavaService:

  def metricsStreamProvider(configDirectory: Option[Path])
  : (addAttribute: String) => fs2.Stream[fs2.Pure, fs2.Chunk[Byte]] =
    val adapter = new PrometheusJmxAdapter(configDirectory)
    addAttribute => adapter.metricsStream(addAttribute = addAttribute)
