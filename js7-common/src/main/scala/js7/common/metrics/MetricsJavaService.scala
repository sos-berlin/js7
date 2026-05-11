package js7.common.metrics

import java.nio.file.Path
import js7.base.data.ByteSequence

trait MetricsJavaService:

  /** @param configDirectory Path of the 'config' directory, if any
    */
  def toMetrics[ByteSeq: ByteSequence](configDirectory: Option[Path]): () => ByteSeq
