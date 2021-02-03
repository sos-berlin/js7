package js7.executor.process

import java.io.{InputStream, InputStreamReader, Reader, Writer}
import js7.base.system.OperatingSystem.isUnix
import js7.base.utils.ScalaUtils.syntax.RichThrowable
import js7.common.process.Processes._
import js7.common.scalautil.FileUtils.syntax._
import js7.common.scalautil.Futures.promiseFuture
import js7.common.scalautil.IOExecutor.ioFuture
import js7.common.scalautil.{IOExecutor, Logger}
import js7.data.job.{CommandLine, ReturnCode}
import js7.executor.process.RichProcess.{startProcessBuilder, tryDeleteFile}
import js7.executor.task.StdChannels
import scala.annotation.tailrec
import scala.concurrent.{ExecutionContext, Future}
import scala.jdk.CollectionConverters._
import scala.util.control.NonFatal
import scala.util.{Success, Try}

/**
  * @author Joacim Zschimmer
  */
class ShellScriptProcess private(
  processConfiguration: ProcessConfiguration,
  process: Process,
  private[process] val commandLine: CommandLine,
  argumentsForLogging: Seq[String])
  (implicit iox: IOExecutor, ec: ExecutionContext)
extends RichProcess(processConfiguration, process, argumentsForLogging)
{
  stdin.close() // Process gets an empty stdin
}

object ShellScriptProcess
{
  private val logger = Logger(getClass)

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
      new ShellScriptProcess(processConfiguration, process, CommandLine.fromFile(shellFile),
        argumentsForLogging = shellFile.toString :: Nil)
      {
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

  def startPipedShellScript(commandLine: CommandLine, conf: ProcessConfiguration, stdChannels: StdChannels)
    (implicit ec: ExecutionContext, iox: IOExecutor): ShellScriptProcess =
  {
    val processBuilder = new ProcessBuilder(toShellCommandArguments(
      commandLine.file, commandLine.arguments.tail ++ conf.idArgumentOption/*TODO Should not be an argument*/).asJava)
    for (o <- conf.workingDirectory) processBuilder.directory(o.toFile)
    processBuilder.environment.putAll(conf.additionalEnvironment.asJava)
    val process = processBuilder.startRobustly()

    def startCopy(in: InputStream, w: Writer): Future[Try[Unit]] =
      ioFuture {
        Try {
          Success(copyChunks(new InputStreamReader(in, conf.encoding), stdChannels.charBufferSize, w))
        }
      }

    val stdoutClosed = startCopy(process.getInputStream, stdChannels.stdoutWriter)
    val stderrClosed = startCopy(process.getErrorStream, stdChannels.stderrWriter)

    new ShellScriptProcess(conf, process, commandLine, argumentsForLogging = commandLine.toString :: Nil) {
      override def terminated = for {
        outTried <- stdoutClosed
        errTried <- stderrClosed
        returnCode <- super.terminated
        returnCode <-
          if (isKilled || isUnix && returnCode.isProcessSignal/*externally killed?*/) {
            for (t <- outTried.failed) logger.warn(t.toStringWithCauses)
            for (t <- errTried.failed) logger.warn(t.toStringWithCauses)
            Future.successful(returnCode)
          } else
            Future.fromTry(for (_ <- outTried; _ <- errTried) yield returnCode)
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
