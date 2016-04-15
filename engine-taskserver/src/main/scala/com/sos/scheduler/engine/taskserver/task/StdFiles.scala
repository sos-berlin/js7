package com.sos.scheduler.engine.taskserver.task

import com.google.common.base.Splitter
import com.sos.scheduler.engine.common.process.StdoutStderr._
import com.sos.scheduler.engine.data.log.SchedulerLogLevel
import com.sos.scheduler.engine.taskserver.data.TaskServerConfiguration._
import com.sos.scheduler.engine.taskserver.task.StdFiles._
import java.nio.charset.Charset
import java.nio.file.Path
import scala.collection.JavaConversions._

/**
 * Stdout and stderr file paths, encoding, stderr SchedulerLogLevel and logging function.
 *
 * @author Joacim Zschimmer
 */
private[task] final case class StdFiles(
  stdFileMap: Map[StdoutStderrType, Path],
  encoding: Charset = Encoding,
  stderrLogLevel: SchedulerLogLevel,
  log: (SchedulerLogLevel, String) ⇒ Unit)
{
  val toLevel = Map(Stdout → SchedulerLogLevel.info, Stderr → stderrLogLevel)

  def output(t: StdoutStderrType, lines: String): Unit = {
    val prefixedLines = stderrLogLevel match {
      case SchedulerLogLevel.info ⇒ prefixLinesWithStdoutOrStderr(t, lines)
      case _ ⇒ lines
    }
    log(toLevel(t), prefixedLines)
  }

  def nonEmpty = !isEmpty
  def isEmpty = stdFileMap.isEmpty  // With no files, all other arguments are not used.
}

object StdFiles {
  private val LineSplitter = Splitter on '\n'

  private[task] def prefixLinesWithStdoutOrStderr(t: StdoutStderrType, lines: String): String =
    LineSplitter split (lines stripSuffix "\n" stripSuffix "\r")  map { o ⇒ s"[${t.string}] $o"} mkString "\n"
}
