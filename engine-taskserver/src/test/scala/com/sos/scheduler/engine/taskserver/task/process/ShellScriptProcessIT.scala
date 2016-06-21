package com.sos.scheduler.engine.taskserver.task.process

import com.sos.scheduler.engine.common.scalautil.Futures.implicits.RichFutures
import com.sos.scheduler.engine.common.system.OperatingSystem.isWindows
import com.sos.scheduler.engine.common.time.ScalaTime._
import com.sos.scheduler.engine.common.time.WaitForCondition.waitForCondition
import com.sos.scheduler.engine.data.job.ReturnCode
import com.sos.scheduler.engine.taskserver.task.process.ShellScriptProcess.startShellScript
import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.junit.JUnitRunner
import scala.concurrent.forkjoin.ForkJoinPool
import scala.concurrent.{ExecutionContext, Future}

/**
  * @author Joacim Zschimmer
  */
@RunWith(classOf[JUnitRunner])
final class ShellScriptProcessIT extends FreeSpec {

  private val n = 1000
  private val threadCount = 10 * sys.runtime.availableProcessors
  private val script = if (isWindows) "@echo off\n" + "exit" else "exit"

  s"$n concurrent process starts with $threadCount threads (JS-1581)" in {
    // Handling "Text file busy" when starting many processes.
    val forkJoinPool = new ForkJoinPool(threadCount)
    implicit val executionContext = ExecutionContext.fromExecutor(forkJoinPool)
    val processFutures = for (i ← 0 until n) yield Future {
      startShellScript(ProcessConfiguration(), name = s"#$i", scriptString = script)
    }
    val processes = processFutures await 300.s
    waitForCondition(300.s, 100.ms) { !(processes exists { _.isAlive }) }
    for (p ← processes) {
      val rc = p.waitForTermination()
      assert(rc == ReturnCode(0))
      p.close()
    }
    processes map { _.scriptFileDeleted } await 60.s
    forkJoinPool.shutdown()
  }
}
