package com.sos.jobscheduler.taskserver.task.process

import com.sos.jobscheduler.base.generic.Completed
import com.sos.jobscheduler.common.process.Processes._
import com.sos.jobscheduler.common.scalautil.FileUtils.implicits._
import com.sos.jobscheduler.common.scalautil.Futures.{namedThreadFuture, promiseFuture}
import com.sos.jobscheduler.data.job.ReturnCode
import com.sos.jobscheduler.taskserver.task.process.RichProcess._
import java.io.{InputStreamReader, Reader, Writer}
import java.nio.file.Path
import scala.annotation.tailrec
import scala.collection.JavaConverters._
import scala.concurrent.{ExecutionContext, Future}
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

  stdin.close() // Process gets an empty stdin
}

object ShellScriptProcess {
  def startShellScript(
    processConfiguration: ProcessConfiguration = ProcessConfiguration(),
    name: String = "shell-script",
    scriptString: String)
    (implicit executionContext: ExecutionContext)
  : ShellScriptProcess = {
    val shellFile = newTemporaryShellFile(name)
    try {
      shellFile.write(scriptString, processConfiguration.encoding)
      val process = startProcessBuilder(processConfiguration, shellFile, arguments = Nil) { _.startRobustly() }
      new ShellScriptProcess(processConfiguration, process, shellFile, argumentsForLogging = shellFile.toString :: Nil) {
        override val terminated = promiseFuture[ReturnCode] { p ⇒
          super.terminated onComplete { o ⇒
            tryDeleteFile(shellFile)
            p.complete(o)
          }
        }
      }
    }
    catch { case NonFatal(t) ⇒
      tryDeleteFile(shellFile)
      throw t
    }
  }

  def startPipedShellScript(
    shellFile: Path,
    processConfiguration: ProcessConfiguration = ProcessConfiguration(),
    stdChannels: StdChannels)
    (implicit executionContext: ExecutionContext): ShellScriptProcess =
  {
    val processBuilder = new ProcessBuilder(toShellCommandArguments(shellFile, processConfiguration.idArgumentOption.toList).asJava)
    processBuilder.environment.putAll(processConfiguration.additionalEnvironment.asJava)
    val process = processBuilder.startRobustly()
    import stdChannels.{charBufferSize, stderrWriter, stdoutWriter}
    val stdoutCompleted = readerTo(new InputStreamReader(process.getInputStream, processConfiguration.encoding), charBufferSize, stdoutWriter)
    val stderrCompleted = readerTo(new InputStreamReader(process.getErrorStream, processConfiguration.encoding), charBufferSize, stderrWriter)

    new ShellScriptProcess(processConfiguration, process, shellFile, argumentsForLogging = shellFile.toString :: Nil) {
      override def terminated = for {
        returnCode ← super.terminated
        _ ← stdoutCompleted
        _ ← stderrCompleted
      } yield returnCode
    }
  }

  private def readerTo(reader: Reader, charBufferSize: Int, writer: Writer)(implicit ec: ExecutionContext): Future[Completed] =
    namedThreadFuture("stdout/stdin reader") {
      forEachChunkOfReader(reader, charBufferSize, writer)
      Completed
    }

  private def forEachChunkOfReader(reader: Reader, charBufferSize: Int, writer: Writer): Unit = {
    val array = new Array[Char](charBufferSize)

    @tailrec def loop(): Unit =
      reader.read(array) match {
        case -1 ⇒
        case len ⇒
          writer.write(array, 0, len)
          loop()
      }

    try loop()
    finally writer.close()  // End of file reached
  }
}
