package js7.service.prometheus

import java.nio.file.Path
import js7.base.data.ByteSequence
import js7.common.metrics.MetricsJavaService

final class PrometheusMetricsJavaService extends MetricsJavaService:

  def toMetrics[ByteSeq: ByteSequence](configDirectory: Option[Path]): () => ByteSeq =
    val adapter = new PrometheusJmxAdapter(configDirectory)
    adapter.metrics
