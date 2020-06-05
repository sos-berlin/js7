package js7.taskserver.task.process

import js7.common.process.Processes._
import js7.common.scalautil.FileUtils.syntax._
import js7.common.scalautil.Futures.promiseFuture
import js7.common.scalautil.IOExecutor
import js7.common.scalautil.IOExecutor.ioFuture
import js7.data.job.ReturnCode
import js7.taskserver.task.process.RichProcess._
import java.io.{InputStream, InputStreamReader, Reader, Writer}
import java.nio.file.Path
import scala.annotation.tailrec
import scala.concurrent.ExecutionContext
import scala.jdk.CollectionConverters._
import scala.util.control.NonFatal

/**
  * @author Joacim Zschimmer
  */
class ShellScriptProcess private(
  processConfiguration: ProcessConfiguration,
  process: Process,
  private[process] val temporaryScriptFile: Path,
  argumentsForLogging: Seq[String])
  (implicit iox: IOExecutor, ec: ExecutionContext)
extends RichProcess(processConfiguration, process, argumentsForLogging) {

  stdin.close() // Process gets an empty stdin
}

object ShellScriptProcess
{
  def startShellScript(
    processConfiguration: ProcessConfiguration,
    name: String = "shell-script",
    scriptString: String)
    (implicit ec: ExecutionContext, iox: IOExecutor)
  : ShellScriptProcess = {
    val shellFile = newTemporaryShellFile(name)
    try {
      shellFile.write(scriptString, processConfiguration.encoding)
      val process = startProcessBuilder(processConfiguration, shellFile, arguments = Nil) { _.startRobustly() }
      new ShellScriptProcess(processConfiguration, process, shellFile, argumentsForLogging = shellFile.toString :: Nil) {
        override val terminated = promiseFuture[ReturnCode] { p =>
          super.terminated onComplete { o =>
            tryDeleteFile(shellFile)
            p.complete(o)
          }
        }
      }
    }
    catch { case NonFatal(t) =>
      tryDeleteFile(shellFile)
      throw t
    }
  }

  def startPipedShellScript(shellFile: Path, conf: ProcessConfiguration, stdChannels: StdChannels)
    (implicit ec: ExecutionContext, iox: IOExecutor): ShellScriptProcess =
  {
    val processBuilder = new ProcessBuilder(toShellCommandArguments(shellFile, conf.idArgumentOption.toList).asJava)
    for (o <- conf.workingDirectory) processBuilder.directory(o.toFile)
    processBuilder.environment.putAll(conf.additionalEnvironment.asJava)
    val process = processBuilder.startRobustly()
    def copy(in: InputStream, w: Writer) = copyChunks(new InputStreamReader(in, conf.encoding), stdChannels.charBufferSize, w)
    val stdoutClosed = ioFuture {
      copy(process.getInputStream, stdChannels.stdoutWriter)
    }
    val stderrClosed = ioFuture {
      copy(process.getErrorStream, stdChannels.stderrWriter)
    }

    new ShellScriptProcess(conf, process, shellFile, argumentsForLogging = shellFile.toString :: Nil) {
      override def terminated = for {
        _ <- stdoutClosed
        _ <- stderrClosed
        returnCode <- super.terminated
      } yield returnCode
    }
  }

  private def copyChunks(reader: Reader, charBufferSize: Int, writer: Writer): Unit = {
    val array = new Array[Char](charBufferSize)

    @tailrec def loop(): Unit =
      reader.read(array) match {
        case -1 =>
        case len =>
          writer.write(array, 0, len)
          loop()
      }

    try loop()
    finally writer.close()  // Send "end of file"
  }
}
