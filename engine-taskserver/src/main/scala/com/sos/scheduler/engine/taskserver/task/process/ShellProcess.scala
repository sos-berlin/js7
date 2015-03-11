package com.sos.scheduler.engine.taskserver.task.process

import com.sos.scheduler.engine.common.scalautil.AutoClosing.autoClosing
import com.sos.scheduler.engine.common.scalautil.HasCloser
import com.sos.scheduler.engine.common.system.OperatingSystem.isWindows
import com.sos.scheduler.engine.common.time.ScalaJoda._
import com.sos.scheduler.engine.data.job.ResultCode
import com.sos.scheduler.engine.taskserver.task.process.ShellProcess._
import com.sos.scheduler.engine.taskserver.task.process.StdoutStderr.{Stdout, StdoutStderrType}
import java.nio.charset.Charset
import java.nio.file.Path
import java.util.concurrent.TimeUnit
import scala.collection.immutable
import scala.concurrent.Promise

/**
 * @author Joacim Zschimmer
 */
final class ShellProcess private[process](process: Process, processFile: Path, stdFiles: Map[StdoutStderrType, Path], fileEncoding: Charset) extends HasCloser {

  private val promise = Promise[Unit]()
  def closedFuture = promise.future

  private val firstLineCollector = new FirstLineCollector
  def firstStdoutLine = firstLineCollector.firstStdoutLine

  override def close(): Unit = {
    try super.close()
    finally promise.success(())
  }

  def waitForTermination(logOutputLine: String ⇒ Unit) = {
    autoClosing(new FilesLineCollector(Nil ++ stdFiles.values, fileEncoding)) { fileLogger ⇒
      def logOutputLines() = fileLogger.nextLinesIterator foreach { case (file, line) ⇒
        logOutputLine(line)
        firstLineCollector.apply(file, line)
      }
      while (!process.waitFor(LoggingPausePeriod.getMillis, TimeUnit.MILLISECONDS)) {     // Die waitFor-Implementierung fragt millisekündlich ab
        logOutputLines()
      }
      logOutputLines()
    }
    ResultCode(process.exitValue)
  }

  override def toString = s"$process $processFile"

  def files: immutable.Seq[Path] = List(processFile) ++ stdFiles.values

  private class FirstLineCollector {
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

object ShellProcess {
  private val LoggingPausePeriod = 1.s
}
