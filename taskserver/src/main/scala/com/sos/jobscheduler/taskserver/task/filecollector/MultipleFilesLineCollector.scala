package com.sos.jobscheduler.taskserver.task.filecollector

import com.sos.jobscheduler.common.scalautil.AutoClosing.closeOnError
import com.sos.jobscheduler.common.scalautil.Closers.implicits.RichClosersAutoCloseable
import com.sos.jobscheduler.common.scalautil.HasCloser
import java.nio.charset.Charset
import java.nio.file.Path
import scala.collection.immutable

/**
 * @author Joacim Zschimmer
 */
final class MultipleFilesLineCollector[Key](sources: immutable.Iterable[(Key, Path)], encoding: Charset, batchThreshold: Int)
extends HasCloser {

  private val lineCollectors = closeOnError(closer) {
    sources map { o ⇒ o → new FileLineCollector(o._2, encoding, batchThreshold = batchThreshold).closeWithCloser }
  }

  def nextBatchIterator: Iterator[((Key, Path), immutable.Seq[String])] =
    lineCollectors.iterator flatMap { case (source, collector) ⇒ collector.nextBatchIterator map { source → _ } }

  override def toString = s"${getClass.getSimpleName}($sources)"
}
