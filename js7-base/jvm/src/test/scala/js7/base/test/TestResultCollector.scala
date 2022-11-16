package js7.base.test

import js7.base.thread.VirtualThreads.newMaybeVirtualThread
import scala.collection.mutable

private object TestResultCollector
{
  private val sb = new mutable.StringBuilder("\n")

  sys.runtime.addShutdownHook(
    newMaybeVirtualThread { () =>
      synchronized {
        println(sb)
      }
    })

  def add(result: LoggingTestAdder.Result): Unit =
    synchronized {
      if (result.tried.isSuccess) sb.append("✔️ ")
      sb.append(result.toLogLine)
      sb.append("\n")
    }
}
