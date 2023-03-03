package js7.base.test

import js7.base.log.{Log4j, Logger}
import js7.base.system.Java8Polyfill.*
import js7.base.test.LoggingTestAdder.Result
import js7.base.thread.VirtualThreads.newMaybeVirtualThread
import js7.base.utils.ScalaUtils.syntax.RichBoolean
import js7.base.utils.Tests.isIntelliJIdea
import scala.collection.mutable
import scala.jdk.CollectionConverters.MapHasAsScala

private object TestResultCollector
{
  private val results = mutable.Buffer[Result]()
  private val logger = Logger[this.type]
  private val ThreadNameRegex = """(\d+-)?(.*)""".r

  sys.runtime.addShutdownHook(
    newMaybeVirtualThread("TestResultCollector-Shutdown") {
      logThreads()
      logTestSummary()
      Log4j.shutdown() // Set shutdownHook="disable" in project/log4j2.xml !!!
    })

  private def logThreads(): Unit =
    if (logger.underlying.isDebugEnabled) {
      val threadToTrace = Thread.getAllStackTraces.asScala
        .toVector
        .sortBy(th => (th._1.getName, th._1.threadId))

      logger.debug(threadToTrace.size.toString + " threads:\n" +
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
          .mkString("\n"))

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
    }

  private def logTestSummary(): Unit = {
    val summary = asString
    logger.info(s"Test summary:\n$summary\n")
    if (isIntelliJIdea) {
      println(summary)
    }
  }

  def add(result: Result): Unit =
    synchronized {
      results += result
    }

  def asString: String =
    synchronized {
      results
        .sortWith((a, b) =>
          if (a.prettyDuration != b.prettyDuration)
            a.duration < b.duration
          else
            a.prefix.compareTo(b.prefix) match {
              case 0 => a.testName == b.testName
              case i => i < 0
            })
        .map(_.toSummaryLine)
        .mkString("\n")
    }

  java8Polyfill()
}
