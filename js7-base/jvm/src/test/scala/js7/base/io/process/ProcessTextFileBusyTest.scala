package js7.base.io.process

import cats.effect.IO
import cats.effect.unsafe.IORuntime
import java.io.IOException
import java.nio.file.Files.delete
import java.nio.file.Path
import js7.base.io.file.FileUtils.syntax.RichPath
import js7.base.io.process.ProcessTextFileBusyTest.*
import js7.base.io.process.Processes.*
import js7.base.io.process.StartRobustly.startRobustly
import js7.base.log.Logger
import js7.base.system.OperatingSystem.isWindows
import js7.base.test.OurTestSuite
import js7.base.thread.CatsBlocking.syntax.await
import js7.base.time.ScalaTime.*
import js7.base.time.Stopwatch
import js7.base.utils.ScalaUtils.syntax.RichThrowable
import scala.util.control.NonFatal

/**
  * JS-1581 "Text file busy" when starting many processes.
  *
  * @author Joacim Zschimmer
  * @see https://bugs.openjdk.java.net/browse/JDK-8068370
  */
final class ProcessTextFileBusyTest extends OurTestSuite:

  private given IORuntime = ioRuntime

  private val n = 1000

  "TextFileBusyIOException" in:
    val (expected, exceptions) = List(
      true -> new IOException("xx  error=26, Text file busy"),
      true -> new IOException("xx  error=26, Das Programm kann nicht ausgeführt oder verändert werden (busy)"),
      true -> new IOException("error=26"),
      false -> new IOException("error=261")
    ).unzip
    val r = for e <- exceptions yield e match
      case StartRobustly.TextFileBusyIOException(x) => assert(x eq e); true
      case _ => false
    assert(r == expected)

  s"$n concurrent process starts with freshly written executables" in:
    //implicit val scheduler = Scheduler.forkJoin(parallelism = n, maxThreads = n)
    val stopwatch = new Stopwatch
    val (files, processes) =
      IO.parSequenceN(sys.runtime.availableProcessors):
        (1 to n).toVector.map: i =>
          IO.defer:
            val file = newTemporaryShellFile(s"#$i")
            file := "exit"
            new ProcessBuilder(s"$file").startRobustly().map(file -> _)
      .await(99.s)
      .unzip

    for p <- processes do
      val rc = p.waitFor()
      assert(rc == 0)

    info(stopwatch.itemsPerSecondString(n, "processes"))
    if isWindows then sleep(500.ms)  // Windows may lock the files for a short while after process termination
    files foreach tryDelete


object ProcessTextFileBusyTest:
  private val logger = Logger[this.type]

  private def tryDelete(path: Path): Unit =
    // Under Windows, the file may be locked for a very short while, resulting in error
    // "The process cannot access the file because it is being used by another process.".
    try delete(path)
    catch
      case NonFatal(t) => logger.warn(s"$path: ${t.toStringWithCauses}")
