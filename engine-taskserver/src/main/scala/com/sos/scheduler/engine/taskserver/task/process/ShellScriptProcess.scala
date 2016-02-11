package com.sos.scheduler.engine.taskserver.task.process

import com.sos.scheduler.engine.common.scalautil.FileUtils.implicits._
import com.sos.scheduler.engine.taskserver.data.TaskServerConfiguration.Encoding
import com.sos.scheduler.engine.taskserver.task.process.Processes._
import com.sos.scheduler.engine.taskserver.task.process.RichProcess._
import java.nio.file.Path
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{ExecutionContext, Promise}
import scala.util.control.NonFatal

/**
  * @author Joacim Zschimmer
  */
final class ShellScriptProcess(
  processConfiguration: ProcessConfiguration,
  process: Process,
  private[process] val temporaryScriptFile: Path)
extends RichProcess(processConfiguration, process) {

  private val scriptFileDeletedPromise = Promise[Boolean]()

  def scriptFileDeleted = scriptFileDeletedPromise.future

  stdin.close() // Process gets an empty stdin
  closed.onComplete { _ ⇒
    val deleted = tryDeleteFiles(List(temporaryScriptFile))
    scriptFileDeletedPromise.success(deleted)
  }
}

object ShellScriptProcess {
  def startShellScript(
    processConfiguration: ProcessConfiguration = ProcessConfiguration(),
    name: String = "shell-script",
    scriptString: String)
    (implicit exeuctionContext: ExecutionContext): ShellScriptProcess =
  {
    val shellFile = newTemporaryShellFile(name)
    try {
      shellFile.write(scriptString, Encoding)
      val conf = processConfiguration.copy(fileOption = Some(shellFile))
      val process  = startProcessBuilder(conf, shellFile) { _.startRobustly() }
      new ShellScriptProcess(conf, process, shellFile)
    }
    catch { case NonFatal(t) ⇒
      RichProcess.tryDeleteFiles(List(shellFile))
      throw t
    }
  }
}
