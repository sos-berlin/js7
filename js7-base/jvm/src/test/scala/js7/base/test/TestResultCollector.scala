package js7.base.test

import java.io.IOException
import java.nio.file.Files.deleteIfExists
import java.nio.file.Paths
import js7.base.log.{Log4j, Logger}
import js7.base.system.Java8Polyfill.*
import js7.base.system.JavaHeapDump.dumpHeapTo
import js7.base.test.LoggingTestAdder.Result
import js7.base.thread.VirtualThreads.newMaybeVirtualThread
import js7.base.utils.ScalaUtils.syntax.{RichBoolean, RichThrowable}
import org.scalatest.exceptions.TestPendingException
import scala.collection.mutable
import scala.jdk.CollectionConverters.MapHasAsScala
import scala.util.control.NonFatal
import scala.util.{Failure, Success, Try}

private object TestResultCollector:
  val singleton = this

  private val results = mutable.Buffer[Result]()
  private val logger = Logger[this.type]
  private val ThreadNameRegex = """(\d+-)?(.*)""".r
  private val dumpFile = Paths.get("target/test.hprof")

  try
    deleteIfExists(Paths.get(s"$dumpFile.idom")) // YourKit Java profiler file
    deleteIfExists(dumpFile)
  catch { case e: IOException => logger.warn(e.toStringWithCauses) }

  sys.runtime.addShutdownHook(
    newMaybeVirtualThread("TestResultCollector-shutdown-hook") {
      logThreads()
      logger.info(s"Test summary:\n$asString\n")
      if sys.props.contains("js7.dumpHeap") then dumpJavaHeap()
      if false then Log4j.shutdown() // Set shutdownHook="disable" in project/log4j2.xml !!!
    })

  private def logThreads(): Unit =
    if logger.underlying.isDebugEnabled then
      val threadToTrace = Thread.getAllStackTraces.asScala
        .toVector
        .sortBy(th => (th._1.getName, th._1.threadId))

      val msg = "Shutdown hook\n" + threadToTrace.size.toString + " threads:\n" +
        "━" * 80 + "\n" +
        threadToTrace.view.map(_._1.getName)
          .groupBy(name => ThreadNameRegex.findAllIn(name.reverse).group(2).reverse)
          .view
          .map { case (groupName, names) =>
            val n = names.size
            "- " + groupName + ((n > 1) ?? s" $n×")
          }
          .toVector
          .sorted
          .mkString("\n")

      try logger.debug(msg)
      catch case NonFatal(t) => System.err.println(s"TestResultCollector: ${t.toStringWithCauses}")

      //logger.debug(threadToTrace.size.toString + " threads:\n" +
      //  "━" * 80 + "\n" +
      //  threadToTrace.view
      //    .map(_._1)
      //    .map(th => s"  ${th.getName} #${th.threadId} ${th.getState}")
      //    .mkString("\n"))

      //for (case (t, stack) <- threadToTrace) {
      //  logger.trace(s"Thread ${t.getName} #${t.threadId} ${t.getState}\n" +
      //    stack.view.map(o => s"  $o").mkString("\n"))
      //}

  private def dumpJavaHeap(): Unit =
    try dumpHeapTo(dumpFile)
    catch { case _: IOException =>
      // sbt seems to start separate ClassLoader (for each subproject?).
      // Each ClassLoader has its own ShutdownHook which
      // all are started simultaneously when JVM terminates.
      // So we ignore multiple calls and
      // detect this via the existing dump file created by the first call.
    }

  def add(result: Result): Unit =
    synchronized:
      results += result

  def asString: String =
    synchronized:
      results
        .sortWith { (a, b) =>
          val at = weighTry(a.tried)
          val bt = weighTry(b.tried)
          if at != bt then
            at < bt
          else if a.prettyDuration != b.prettyDuration then
            a.duration < b.duration
          else
            a.prefix.compareTo(b.prefix) match
              case 0 => a.testName == b.testName
              case i => i < 0
        }
        .map(_.toSummaryLine)
        .mkString("\n")

  private def weighTry(tried: Try[Unit]): Int =
    tried match
      case Success(_) => 1
      case Failure(_: TestPendingException) => 2
      case Failure(_) => 3

  java8Polyfill()
