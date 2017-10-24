package com.sos.jobscheduler.taskserver.common

import com.sos.jobscheduler.common.async.ConcurrentCaller
import com.sos.jobscheduler.common.scalautil.Closers.implicits.RichClosersAutoCloseable
import com.sos.jobscheduler.common.scalautil.{ClosedFuture, HasCloser}
import java.time.Duration
import scala.concurrent.Future

/**
 * @author Joacim Zschimmer
 */
final class ConcurrentStdoutAndStderrWell(name: String, stdFiles: StdFiles, pollPeriod: Duration, logBatchThreshold: Int)
extends HasCloser with ClosedFuture {

  private val well = new StdoutStderrWell(stdFiles.stdFileMap, stdFiles.encoding, batchThreshold = logBatchThreshold, stdFiles.output).closeWithCloser
  private val concurrentCaller = new ConcurrentCaller(
    pauses = Iterator continually pollPeriod,
    function = () ⇒ well.apply(),
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
