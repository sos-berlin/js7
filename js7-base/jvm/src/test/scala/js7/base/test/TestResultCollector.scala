package js7.base.test

import js7.base.log.Logger
import js7.base.test.LoggingTestAdder.Result
import js7.base.thread.VirtualThreads.newMaybeVirtualThread
import js7.base.utils.Tests.isIntelliJIdea
import scala.collection.mutable

private object TestResultCollector
{
  private val results = mutable.Buffer[Result]()
  private val logger = Logger[this.type]

  sys.runtime.addShutdownHook(
    newMaybeVirtualThread("TestResultCollector-Shutdown") {
      val summary = asString
      logger.info(s"Test summary:\n$summary")
      if (isIntelliJIdea) {
        println(summary)
      }
    })

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
}
