package js7.common.metrics

import cats.effect.IO
import java.nio.file.Path
import org.apache.pekko.util.ByteString

trait MetricsJavaService:

  /** @param configDirectory Path of the 'config' directory, if any
    */
  def metricsLines(configDirectory: Option[Path])
  : (addAttribute: String) => fs2.Stream[IO, ByteString]
