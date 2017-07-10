package com.sos.jobscheduler.taskserver.task.process

import akka.util.ByteString
import com.sos.jobscheduler.base.generic.Completed
import com.sos.jobscheduler.common.process.Processes._
import com.sos.jobscheduler.common.scalautil.FileUtils.implicits._
import com.sos.jobscheduler.data.system.StdoutStderr.{Stderr, Stdout, StdoutStderrType}
import com.sos.jobscheduler.taskserver.data.TaskServerConfiguration.Encoding
import com.sos.jobscheduler.taskserver.task.process.RichProcess._
import java.io.{InputStreamReader, Reader}
import java.nio.file.Path
import scala.annotation.tailrec
import scala.collection.JavaConversions._
import scala.concurrent.{ExecutionContext, Future, Promise, blocking}
import scala.util.control.NonFatal

/**
  * @author Joacim Zschimmer
  */
class ShellScriptProcess private(
  processConfiguration: ProcessConfiguration,
  process: Process,
  private[process] val temporaryScriptFile: Path,
  argumentsForLogging: Seq[String])
  (implicit executionContext: ExecutionContext)
extends RichProcess(processConfiguration, process, argumentsForLogging) {

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
    (implicit executionContext: ExecutionContext): ShellScriptProcess =
  {
    val shellFile = newTemporaryShellFile(name)
    try {
      shellFile.write(scriptString, Encoding)
      val conf = processConfiguration.copy(fileOption = Some(shellFile))
      val process  = startProcessBuilder(conf, shellFile, arguments = Nil) { _.startRobustly() }
      new ShellScriptProcess(conf, process, shellFile, argumentsForLogging = shellFile.toString :: Nil)
    }
    catch { case NonFatal(t) ⇒
      RichProcess.tryDeleteFiles(List(shellFile))
      throw t
    }
  }

  def startPipedShellScript(
    processConfiguration: ProcessConfiguration = ProcessConfiguration(),
    name: String = "shell-script",
    script: String,
    stdoutStderrWriter: StdoutStderrWriter)
    (implicit executionContext: ExecutionContext): ShellScriptProcess =
  {
    val shellFile = newTemporaryShellFile(name)
    try {
      shellFile.write(script.trim, Encoding)
      val processBuilder = new ProcessBuilder(toShellCommandArguments(shellFile, processConfiguration.idArgumentOption.toList))
      processBuilder.environment ++= processConfiguration.additionalEnvironment
      val process = processBuilder.startRobustly()
      import stdoutStderrWriter.{chunkSize, writeChunk}
      val stdoutCompleted = readerTo(new InputStreamReader(process.getInputStream, Encoding), chunkSize)(writeChunk(Stdout, _))
      val stderrCompleted = readerTo(new InputStreamReader(process.getErrorStream, Encoding), chunkSize)(writeChunk(Stderr, _))

      val conf = processConfiguration.copy(fileOption = Some(shellFile))
      new ShellScriptProcess(conf, process, shellFile, argumentsForLogging = shellFile.toString :: Nil) {
        override def terminated = for {
          returnCode ← super.terminated
          _ ← stdoutCompleted
          _ ← stderrCompleted
        } yield returnCode
      }
    }
    catch { case NonFatal(t) ⇒
      RichProcess.tryDeleteFiles(List(shellFile))
      throw t
    }
  }

  private def readerTo(reader: Reader, chunkSize: Int)(handleChunk: String ⇒ Unit)(implicit ec: ExecutionContext): Future[Completed] =
    Future {
      blocking {
        forEachChunkOfReader(reader, chunkSize)(handleChunk)
        Completed
      }
    }

  private def forEachChunkOfReader(reader: Reader, chunkSize: Int)(handleChunk: String ⇒ Unit): Unit = {
    val array = new Array[Char](chunkSize)

    @tailrec def loop(): Unit = {
      reader.read(array) match {
        case -1 ⇒
        case len ⇒
          handleChunk(new String(array, 0, len))
          loop()
      }
    }

    loop()
  }
}
