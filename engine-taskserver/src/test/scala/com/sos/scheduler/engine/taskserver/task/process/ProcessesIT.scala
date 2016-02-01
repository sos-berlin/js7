package com.sos.scheduler.engine.taskserver.task.process

import com.sos.scheduler.engine.common.scalautil.FileUtils.implicits.RichPath
import com.sos.scheduler.engine.common.scalautil.Futures.implicits.RichFututes
import com.sos.scheduler.engine.common.time.ScalaTime._
import com.sos.scheduler.engine.common.time.WaitForCondition.waitForCondition
import com.sos.scheduler.engine.taskserver.task.process.Processes._
import java.nio.file.Files.delete
import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.junit.JUnitRunner
import scala.collection.JavaConversions._
import scala.concurrent.forkjoin.ForkJoinPool
import scala.concurrent.{ExecutionContext, Future}

/**
  * JS-1581 "Text file busy" when starting multiple processes.
  * @author Joacim Zschimmer
  * @see https://bugs.openjdk.java.net/browse/JDK-8068370
  */
@RunWith(classOf[JUnitRunner])
final class ProcessesIT extends FreeSpec {

  val n = 2000
  val threadCount = 500

  s"Massive parallel test with $n process starts and $threadCount threads" in {
    val forkJoinPool = new ForkJoinPool(threadCount)
    implicit val executionContext = ExecutionContext.fromExecutor(forkJoinPool)
    val filesAndProcesses = for (i ← 0 until n) yield Future {
      val file = newTemporaryShellFile("TEST")
      file.contentString = "exit"
      val process = new ProcessBuilder(List(s"$file")).start()
      (file, process)
    }
    val (files, processes) = (filesAndProcesses await 60.s).unzip
    waitForCondition(60.s, 100.ms) { !(processes exists { _.isAlive }) }
    for (p ← processes) p.waitFor()
    for (f ← files) delete(f)
    forkJoinPool.shutdown()
  }
}
