package com.sos.jobscheduler.taskserver.common

import com.sos.jobscheduler.common.process.StdoutStderr.{Stdout, StdoutStderrType}
import com.sos.jobscheduler.common.system.OperatingSystem._
import com.sos.jobscheduler.taskserver.task.filecollector.MultipleFilesLineCollector
import java.nio.charset.Charset
import java.nio.file.Path
import scala.collection.immutable

/**
 * Reads stdout and stderr output files and outputs the arrived data then with every `apply`.
 *
 * @author Joacim Zschimmer
 */
final class StdoutStderrWell(
  stdFiles: Map[StdoutStderrType, Path],
  fileEncoding: Charset,
  batchThreshold: Int,
  output: (StdoutStderrType, immutable.Seq[String]) ⇒ Unit)
extends AutoCloseable {

  private val lineCollector = new MultipleFilesLineCollector(Nil ++ stdFiles, fileEncoding, batchThreshold = batchThreshold)
  private val firstLineCollector = new FirstStdoutLineCollector
  def firstStdoutLine = firstLineCollector.firstStdoutLine

  def close() = lineCollector.close()

  def apply() = synchronized {
    lineCollector.nextBatchIterator foreach { case ((typ, file), batch) ⇒
      output(typ, batch)
      firstLineCollector.apply(file, batch.head)
    }
  }

  private class FirstStdoutLineCollector {
    private val maxLineNr = if (isWindows) 2 else 1  // Windows stdout may start with an empty first line
    private var stdoutLineNr = 0
    var firstStdoutLine = ""

    def apply(file: Path, line: String) =
      if (firstStdoutLine.isEmpty) {
        if (file == stdFiles(Stdout)) {
          stdoutLineNr += 1
          if (stdoutLineNr <= maxLineNr)
            firstStdoutLine = line
        }
      }
  }
}
