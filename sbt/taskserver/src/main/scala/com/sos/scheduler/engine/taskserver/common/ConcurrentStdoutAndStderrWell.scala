package com.sos.scheduler.engine.taskserver.common

import com.sos.scheduler.engine.common.async.ConcurrentCaller
import com.sos.scheduler.engine.common.scalautil.Closers.implicits.RichClosersAutoCloseable
import com.sos.scheduler.engine.common.scalautil.{ClosedFuture, HasCloser}
import com.sos.scheduler.engine.common.time.ScalaTime._
import com.sos.scheduler.engine.taskserver.common.ConcurrentStdoutAndStderrWell._
import scala.concurrent.Future

/**
 * @author Joacim Zschimmer
 */
final class ConcurrentStdoutAndStderrWell(name: String, stdFiles: StdFiles)
extends HasCloser with ClosedFuture {

  private val well = new StdoutStderrWell(stdFiles.stdFileMap, stdFiles.encoding, batchThreshold = LogBatchThreshold, stdFiles.output).closeWithCloser
  private val concurrentCaller = new ConcurrentCaller(
    pauses = Iterator continually PollPeriod,
    function = well.apply,
    name = List("JobScheduler stdout/stderr well", name) filter { _.nonEmpty } mkString " - ")
  .closeWithCloser

  def start() = concurrentCaller.start()

  def finish() = {
    concurrentCaller.close()
    well.apply()
    close()
  }

  def flush(): Unit = well.apply()

  def terminated: Future[Unit] = concurrentCaller.terminated

  def firstStdoutLine = well.firstStdoutLine
}

object ConcurrentStdoutAndStderrWell {
  private val PollPeriod = 5.s
  private val LogBatchThreshold = 1000*1000  // Cut-off count of characters to log (and transfer) at once
}
