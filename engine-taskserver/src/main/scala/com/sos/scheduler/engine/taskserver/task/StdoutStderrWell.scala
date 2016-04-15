package com.sos.scheduler.engine.taskserver.task

import com.sos.scheduler.engine.common.process.StdoutStderr.{Stdout, StdoutStderrType}
import com.sos.scheduler.engine.common.scalautil.Closers.implicits.RichClosersAutoCloseable
import com.sos.scheduler.engine.common.scalautil.HasCloser
import com.sos.scheduler.engine.common.system.OperatingSystem._
import com.sos.scheduler.engine.taskserver.task.filecollector.MultipleFilesLineCollector
import java.nio.charset.Charset
import java.nio.file.Path

/**
 * Reads stdout and stderr output files and outputs the arrived data then with every `apply`.
 *
 * @author Joacim Zschimmer
 */
final class StdoutStderrWell(stdFiles: Map[StdoutStderrType, Path], fileEncoding: Charset, output: (StdoutStderrType, String) ⇒ Unit)
extends HasCloser {

  private val lineCollector = new MultipleFilesLineCollector(Nil ++ stdFiles, fileEncoding).closeWithCloser
  private val firstLineCollector = new FirstStdoutLineCollector
  def firstStdoutLine = firstLineCollector.firstStdoutLine

  def apply() = synchronized {
    lineCollector.nextLinesIterator foreach { case ((typ, file), line) ⇒
      output(typ, line)
      firstLineCollector.apply(file, line)
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
