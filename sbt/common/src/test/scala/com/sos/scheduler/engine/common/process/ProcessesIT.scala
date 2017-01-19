package com.sos.scheduler.engine.common.process

import com.sos.scheduler.engine.common.process.Processes._
import com.sos.scheduler.engine.common.process.ProcessesIT._
import com.sos.scheduler.engine.common.scalautil.FileUtils.implicits.RichPath
import com.sos.scheduler.engine.common.scalautil.Futures.implicits.RichFutures
import com.sos.scheduler.engine.common.scalautil.Logger
import com.sos.scheduler.engine.common.system.OperatingSystem.isWindows
import com.sos.scheduler.engine.common.time.ScalaTime._
import com.sos.scheduler.engine.common.time.Stopwatch
import com.sos.scheduler.engine.common.time.WaitForCondition.waitForCondition
import java.nio.file.Files.delete
import java.nio.file.Path
import org.scalatest.FreeSpec
import scala.collection.JavaConversions._
import scala.concurrent.forkjoin.ForkJoinPool
import scala.concurrent.{ExecutionContext, Future}
import scala.util.control.NonFatal

/**
  * JS-1581 "Text file busy" when starting many processes.
  *
  * @author Joacim Zschimmer
  * @see https://bugs.openjdk.java.net/browse/JDK-8068370
  */
final class ProcessesIT extends FreeSpec {

  private val n = 1000
  private val threadCount = 10 * sys.runtime.availableProcessors

  s"$n concurrent process starts with $threadCount threads" in {
    val forkJoinPool = new ForkJoinPool(threadCount)
    implicit val executionContext = ExecutionContext.fromExecutor(forkJoinPool)
    val stopwatch = new Stopwatch
    val filesAndProcesses = for (i ← 0 until n) yield Future {
      val file = newTemporaryShellFile(s"#$i")
      file.contentString = "exit"
      val process = new ProcessBuilder(List(s"$file")).startRobustly()
      (file, process)
    }
    val (files, processes) = (filesAndProcesses await 300.s).unzip
    waitForCondition(300.s, 100.ms) { !(processes exists { _.isAlive }) }
    logger.info(stopwatch.itemsPerSecondString(n, "process", "processes"))
    for (p ← processes) {
      val rc = p.waitFor()
      assert(rc == 0)
    }
    if (isWindows) sleep(500.ms)  // Windows may lock the files for a short while after process termination
    files foreach tryDelete
    forkJoinPool.shutdown()
  }
}

object ProcessesIT {
  private val logger = Logger(getClass)

  private def tryDelete(path: Path): Unit = {
    // Under Windows, the file may be locked for a very short while, resulting in error
    // "The process cannot access the file because it is being used by another process.".
    try delete(path)
    catch {
      case NonFatal(t) ⇒ logger.warn(s"$path: $t")
    }
  }
}
