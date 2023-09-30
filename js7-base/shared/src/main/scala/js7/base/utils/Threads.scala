package js7.base.utils

import js7.base.utils.ScalaUtils.syntax.*
import scala.jdk.CollectionConverters.*

object Threads:

  def allThreadStackTraces(): Seq[String] =
    Thread.getAllStackTraces.asScala
      .toVector
      .sortBy(_._1.getName)
      .map { case (thread, stacktrace) =>
        val currentMethod = stacktrace.headOption.map(o => s"${o.getClassName}.${o.getMethodName}")
        val stacktraceString = stacktrace.map(o => "\n  " + o.toString).mkString
        "Thread " + thread.getName + currentMethod.fold("")(o => " at " + o) +
          (stacktraceString.nonEmpty ?? " ⏎") +
          stacktraceString
      }
