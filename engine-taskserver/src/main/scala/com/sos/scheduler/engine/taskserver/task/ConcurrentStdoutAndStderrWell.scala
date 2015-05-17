package com.sos.scheduler.engine.taskserver.task

import com.sos.scheduler.engine.common.async.ConcurrentCaller
import com.sos.scheduler.engine.common.scalautil.Closers.implicits.RichClosersAutoCloseable
import com.sos.scheduler.engine.common.scalautil.{ClosedFuture, HasCloser}
import com.sos.scheduler.engine.common.time.ScalaTime._
import com.sos.scheduler.engine.taskserver.task.ConcurrentStdoutAndStderrWell._
import com.sos.scheduler.engine.taskserver.task.process.StdoutStderr.StdoutStderrType
import java.nio.charset.Charset
import java.nio.file.Path
import scala.concurrent.Future

/**
 * @author Joacim Zschimmer
 */
final class ConcurrentStdoutAndStderrWell(name: String, stdFiles: Map[StdoutStderrType, Path], fileEncoding: Charset, output: String ⇒ Unit)
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

object ConcurrentStdoutAndStderrWell {
  private val PollPeriod = 10.s
}
