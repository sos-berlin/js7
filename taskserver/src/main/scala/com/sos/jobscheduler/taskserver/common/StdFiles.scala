package com.sos.jobscheduler.taskserver.common

import com.google.common.base.Splitter
import com.sos.jobscheduler.data.system.{Stderr, Stdout, StdoutOrStderr}
import com.sos.jobscheduler.taskserver.common.StdFiles._
import com.sos.jobscheduler.taskserver.data.SchedulerLogLevel
import com.sos.jobscheduler.taskserver.data.TaskServerConfiguration._
import java.nio.charset.Charset
import java.nio.file.Path
import scala.collection.JavaConverters._
import scala.collection.immutable

/**
 * Stdout and stderr file paths, encoding, stderr SchedulerLogLevel and logging function.
 *
 * @author Joacim Zschimmer
 */
final case class StdFiles(
  stdFileMap: Map[StdoutOrStderr, Path],
  encoding: Charset = Encoding,
  stderrLogLevel: SchedulerLogLevel,
  log: (SchedulerLogLevel, String) ⇒ Unit)
{
  val toLevel = Map(
    Stdout → SchedulerLogLevel.info,
    Stderr → stderrLogLevel)

  def output(t: StdoutOrStderr, batch: immutable.Seq[String]): Unit = {
    val prefixedLines = stderrLogLevel match {
      case SchedulerLogLevel.info ⇒ batch map { lines ⇒ prefixLinesWithStdoutOrStderr(t, lines) }
      case _ ⇒ batch
    }
    log(toLevel(t), prefixedLines mkString "\n")
  }

  def nonEmpty = !isEmpty
  def isEmpty = stdFileMap.isEmpty  // With no files, none of the other arguments is used.
}

object StdFiles {
  private val LineSplitter = Splitter on '\n'

  private[taskserver] def prefixLinesWithStdoutOrStderr(t: StdoutOrStderr, lines: String): String =
    (LineSplitter split (lines stripSuffix "\n" stripSuffix "\r")).asScala
      .map { o ⇒ s"[${t.string}] $o"}
      .mkString("\n")
}
