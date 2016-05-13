package com.sos.scheduler.engine.taskserver.task.filecollector

import com.sos.scheduler.engine.common.scalautil.Closers.implicits.RichClosersAutoCloseable
import com.sos.scheduler.engine.common.scalautil.FileUtils.implicits._
import com.sos.scheduler.engine.common.scalautil.HasCloser
import java.io.{BufferedReader, FileInputStream, InputStreamReader}
import java.nio.charset.Charset
import java.nio.file.Path
import scala.collection.immutable

/**
 * @author Joacim Zschimmer
 */
final class FileLineCollector(file: Path, encoding: Charset, batchThreshold: Int) extends HasCloser {

  private val in = new FileInputStream(file).closeWithCloser
  private val collector = new LineBatchCollector(new BufferedReader(new InputStreamReader(in, encoding)), threshold = batchThreshold)

  /**
   * @return the next batch of all available lines
   */
  def nextBatchIterator: Iterator[immutable.Seq[String]] = collector.nextBatchIterator()

  override def toString = s"${getClass.getSimpleName}($file)"
}
