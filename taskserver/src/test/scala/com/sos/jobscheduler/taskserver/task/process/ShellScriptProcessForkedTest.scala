package js7.taskserver.task.process

import js7.common.scalautil.Futures.implicits._
import js7.common.scalautil.IOExecutor.Implicits.globalIOX
import js7.common.system.OperatingSystem.isWindows
import js7.base.time.ScalaTime._
import js7.common.time.WaitForCondition.waitForCondition
import js7.data.job.ReturnCode
import js7.taskserver.task.process.ShellScriptProcess.startShellScript
import java.util.concurrent.ForkJoinPool
import scala.concurrent.{ExecutionContext, Future}
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class ShellScriptProcessForkedTest extends AnyFreeSpec {

  private val n = 1000
  private val threadCount = 10 * sys.runtime.availableProcessors
  private val script = if (isWindows) "@echo off\n" + "exit" else "exit"

  s"$n concurrent process starts with $threadCount threads (JS-1581)" in {
    // Handling "Text file busy" when starting many processes.
    val forkJoinPool = new ForkJoinPool(threadCount)
    implicit val executionContext = ExecutionContext.fromExecutor(forkJoinPool)
    val processFutures = for (i <- 0 until n) yield Future {
      startShellScript(ProcessConfiguration.forTest, name = s"#$i", scriptString = script)
    }
    val processes = processFutures await 300.s
    waitForCondition(300.s, 100.ms) { !(processes exists { _.isAlive }) }
    for (p <- processes) {
      val rc = p.terminated await 99.s
      assert(rc == ReturnCode(0))
      p.close()
    }
    forkJoinPool.shutdown()
  }
}
