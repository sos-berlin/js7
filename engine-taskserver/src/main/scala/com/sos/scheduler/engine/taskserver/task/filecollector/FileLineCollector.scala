package com.sos.scheduler.engine.taskserver.task.filecollector

import com.sos.scheduler.engine.common.scalautil.Closers.implicits.RichClosersAutoCloseable
import com.sos.scheduler.engine.common.scalautil.FileUtils.implicits._
import com.sos.scheduler.engine.common.scalautil.HasCloser
import java.io.{BufferedReader, FileInputStream, InputStreamReader}
import java.nio.charset.Charset
import java.nio.file.Path

/**
 * @author Joacim Zschimmer
 */
final class FileLineCollector(file: Path, encoding: Charset) extends HasCloser {

  private val in = new FileInputStream(file).closeWithCloser
  private val collector = new LineCollector(new BufferedReader(new InputStreamReader(in, encoding)))

  /**
   * @return the next batch of all available lines
   */
  def nextLinesIterator: Iterator[String] = collector.nextLinesIterator()

  override def toString = s"${getClass.getSimpleName}($file)"
}
