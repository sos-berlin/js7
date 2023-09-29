package js7.base.io.process

import java.nio.file.Files.delete
import java.nio.file.Path
import js7.base.io.file.FileUtils.syntax.RichPath
import js7.base.io.process.ProcessTextFileBusyTest.*
import js7.base.io.process.Processes.*
import js7.base.log.Logger
import js7.base.system.OperatingSystem.isWindows
import js7.base.test.OurTestSuite
import js7.base.thread.MonixBlocking.syntax.RichTask
import js7.base.time.ScalaTime.*
import js7.base.time.Stopwatch
import js7.base.utils.ScalaUtils.syntax.RichThrowable
import monix.eval.Task
import monix.execution.Scheduler
import scala.util.control.NonFatal

/**
  * JS-1581 "Text file busy" when starting many processes.
  *
  * @author Joacim Zschimmer
  * @see https://bugs.openjdk.java.net/browse/JDK-8068370
  */
final class ProcessTextFileBusyTest extends OurTestSuite:

  private val n = 1000
  private val threadCount = 10 * sys.runtime.availableProcessors

  s"$n concurrent process starts with freshly written executables on $threadCount threads" in:
    implicit val scheduler = Scheduler.forkJoin(parallelism = n, maxThreads = n)
    val stopwatch = new Stopwatch
    val (files, processes) = Task
      .parSequenceUnordered(
        for i <- (0 until n) yield Task.defer {
          val file = newTemporaryShellFile(s"#$i")
          file := "exit"
          new ProcessBuilder(s"$file").startRobustly()
            .map(file -> _)
        })
      .await(99.s)
      .unzip
    for p <- processes do
      val rc = p.waitFor()
      assert(rc == 0)
    info(stopwatch.itemsPerSecondString(n, "processes"))
    if isWindows then sleep(500.ms)  // Windows may lock the files for a short while after process termination
    files foreach tryDelete
    scheduler.shutdown()

object ProcessTextFileBusyTest:
  private val logger = Logger[this.type]

  private def tryDelete(path: Path): Unit =
    // Under Windows, the file may be locked for a very short while, resulting in error
    // "The process cannot access the file because it is being used by another process.".
    try delete(path)
    catch
      case NonFatal(t) => logger.warn(s"$path: ${t.toStringWithCauses}")
