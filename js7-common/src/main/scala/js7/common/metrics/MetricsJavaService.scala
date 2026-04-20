package js7.common.metrics

import java.nio.file.Path

trait MetricsJavaService:

  /** @param configDirectory Path of the 'config' directory, if any
    */
  def metricsStreamProvider(configDirectory: Option[Path])
  : (addAttribute: String) => fs2.Stream[fs2.Pure, fs2.Chunk[Byte]]
