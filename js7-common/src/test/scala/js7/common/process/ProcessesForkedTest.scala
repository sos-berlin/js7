package js7.common.process

import java.nio.file.Files.delete
import java.nio.file.Path
import java.util.concurrent.ForkJoinPool
import js7.base.system.OperatingSystem.isWindows
import js7.base.time.ScalaTime._
import js7.base.time.Stopwatch
import js7.common.process.Processes._
import js7.common.process.ProcessesForkedTest._
import js7.common.scalautil.FileUtils.syntax.RichPath
import js7.common.scalautil.Futures.implicits.RichFutures
import js7.common.scalautil.Logger
import js7.common.time.WaitForCondition.waitForCondition
import org.scalatest.freespec.AnyFreeSpec
import scala.concurrent.{ExecutionContext, Future}
import scala.util.control.NonFatal

/**
  * JS-1581 "Text file busy" when starting many processes.
  *
  * @author Joacim Zschimmer
  * @see https://bugs.openjdk.java.net/browse/JDK-8068370
  */
final class ProcessesForkedTest extends AnyFreeSpec {

  private val n = 1000
  private val threadCount = 10 * sys.runtime.availableProcessors

  s"$n concurrent process starts with $threadCount threads" in {
    val forkJoinPool = new ForkJoinPool(threadCount)
    implicit val executionContext = ExecutionContext.fromExecutor(forkJoinPool)
    val stopwatch = new Stopwatch
    val filesAndProcesses = for (i <- 0 until n) yield
      Future {
        val file = newTemporaryShellFile(s"#$i")
        file := "exit"
        val process = new ProcessBuilder(s"$file").startRobustly()
        (file, process)
      }
    val (files, processes) = (filesAndProcesses await 300.s).unzip
    waitForCondition(300.s, 100.ms) { !processes.exists(_.isAlive) }
    info(stopwatch.itemsPerSecondString(n, "processes"))
    for (p <- processes) {
      val rc = p.waitFor()
      assert(rc == 0)
    }
    if (isWindows) sleep(500.ms)  // Windows may lock the files for a short while after process termination
    files foreach tryDelete
    forkJoinPool.shutdown()
  }
}

object ProcessesForkedTest {
  private val logger = Logger(getClass)

  private def tryDelete(path: Path): Unit = {
    // Under Windows, the file may be locked for a very short while, resulting in error
    // "The process cannot access the file because it is being used by another process.".
    try delete(path)
    catch {
      case NonFatal(t) => logger.warn(s"$path: $t")
    }
  }
}
