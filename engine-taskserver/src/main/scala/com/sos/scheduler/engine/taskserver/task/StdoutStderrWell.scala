package com.sos.scheduler.engine.taskserver.task

import com.sos.scheduler.engine.common.async.ConcurrentCaller
import com.sos.scheduler.engine.common.scalautil.Closers.implicits.RichClosersAutoCloseable
import com.sos.scheduler.engine.common.scalautil.{ClosedFuture, HasCloser}
import com.sos.scheduler.engine.common.system.OperatingSystem._
import com.sos.scheduler.engine.common.time.ScalaTime._
import com.sos.scheduler.engine.taskserver.task.common.MultipleFilesLineCollector
import com.sos.scheduler.engine.taskserver.task.process.StdoutStderr.{Stdout, StdoutStderrType}
import java.nio.charset.Charset
import java.nio.file.Path
import scala.concurrent.Future

/**
 * Reads stdout and stderr output files and outputs the arrived data then with every `apply`.
 *
 * @author Joacim Zschimmer
 */
final class StdoutStderrWell(stdFiles: Map[StdoutStderrType, Path], fileEncoding: Charset, output: String ⇒ Unit)
extends HasCloser {

  private val lineCollector = new MultipleFilesLineCollector(Nil ++ stdFiles.values, fileEncoding).closeWithCloser
  private val firstLineCollector = new FirstStdoutLineCollector
  def firstStdoutLine = firstLineCollector.firstStdoutLine

  def apply() = synchronized {
    lineCollector.nextLinesIterator foreach { case (file, line) ⇒
      output(line)
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

object StdoutStderrWell {
  private val PollPeriod = 10.s

  final class Concurrent(name: String, stdFiles: Map[StdoutStderrType, Path], fileEncoding: Charset, output: String ⇒ Unit)
  extends HasCloser with ClosedFuture {

    private val well = new StdoutStderrWell(stdFiles, fileEncoding, output).closeWithCloser
    private val concurrentCaller = new ConcurrentCaller(Iterator continually PollPeriod, well.apply, name).closeWithCloser

    def start() = concurrentCaller.start()

    def finish() = {
      concurrentCaller.close()
      well.apply()
      close()
    }

    def terminated: Future[Unit] = concurrentCaller.terminated

    def firstStdoutLine = well.firstStdoutLine
  }
}
