package com.sos.scheduler.engine.taskserver.task

import com.google.common.io.Closer
import com.sos.scheduler.engine.base.process.ProcessSignal
import com.sos.scheduler.engine.common.scalautil.AutoClosing._
import com.sos.scheduler.engine.common.scalautil.Closers.implicits.RichClosersCloser
import com.sos.scheduler.engine.taskserver.TaskServer
import com.sos.scheduler.engine.taskserver.task.process.{JavaProcess, RichProcess}
import java.io.File
import scala.concurrent.ExecutionContext.Implicits.global
import spray.json._

/**
 * @author Joacim Zschimmer
 */
final class SeparateProcessTaskServer(arguments: TaskStartArguments, javaOptions: Seq[String], javaClasspath: String)
extends TaskServer {

  private var process: RichProcess = null

  def start() = {
    val stdFileMap = RichProcess.createTemporaryStdFiles()
    val closer = Closer.create()
    closeOnError(closer) {
      closer.onClose { RichProcess.tryDeleteFiles(stdFileMap.values) }
      process = JavaProcess.startJava(
        environment = arguments.environment,
        options = javaOptions,
        classpath = Some(javaClasspath + File.pathSeparator + JavaProcess.OwnClasspath),
        mainClass = com.sos.scheduler.engine.taskserver.TaskServerMain.getClass.getName stripSuffix "$", // Strip Scala object class suffix
        arguments = Nil,
        stdFileMap = stdFileMap)
      process.closed.onComplete { _ ⇒ closer.close() }
      val a = arguments.copy(stdFileMap = stdFileMap, logStdoutAndStderr = true)
      process.stdinWriter.write(a.toJson.compactPrint)
      process.stdinWriter.close()
    }
  }

  override def close(): Unit =
    for (p ← Option(process)) {
      try p.waitForTermination()
      finally {
        p.close()
        process = null
      }
    }

  def sendProcessSignal(signal: ProcessSignal) =
    for (p ← Option(process)) p.sendProcessSignal(signal)
}
