package com.sos.scheduler.engine.taskserver.task.common

import com.sos.scheduler.engine.common.scalautil.AutoClosing.closeOnError
import com.sos.scheduler.engine.common.scalautil.Closers.implicits.RichClosersAutoCloseable
import com.sos.scheduler.engine.common.scalautil.HasCloser
import java.nio.charset.Charset
import java.nio.file.Path
import scala.collection.immutable

/**
 * @author Joacim Zschimmer
 */
final class MultipleFilesLineCollector(files: immutable.Iterable[Path], encoding: Charset) extends HasCloser {

  private val lineCollectors = closeOnError(closer) { files map { f ⇒ new FileLineCollector(f, encoding).closeWithCloser } }

  def nextLinesIterator: Iterator[(Path, String)] = lineCollectors.iterator flatMap { c ⇒ c.nextLinesIterator map { c.file → _ } }
}
