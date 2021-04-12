package js7.executor.process

import cats.effect.ExitCase
import java.io.{InputStream, InputStreamReader}
import js7.base.io.file.FileUtils.syntax._
import js7.base.io.process.Processes._
import js7.base.io.process.ReturnCode
import js7.base.log.Logger
import js7.base.monixutils.UnbufferedReaderObservable
import js7.base.system.OperatingSystem.isUnix
import js7.base.thread.Futures.promiseFuture
import js7.base.thread.IOExecutor
import js7.base.utils.ScalaUtils.syntax.RichThrowable
import js7.data.job.CommandLine
import js7.executor.StdObservers
import js7.executor.process.RichProcess.{startProcessBuilder, tryDeleteFile}
import monix.eval.Task
import monix.execution.Scheduler
import monix.reactive.Observable
import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.jdk.CollectionConverters._
import scala.util.control.NonFatal

/**
  * @author Joacim Zschimmer
  */
class ShellScriptProcess private(
  processConfiguration: ProcessConfiguration,
  process: Process,
  argumentsForLogging: Seq[String])
  (implicit ec: ExecutionContext, iox: IOExecutor)
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
      new ShellScriptProcess(processConfiguration, process,
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

  def startPipedShellScript(commandLine: CommandLine, conf: ProcessConfiguration, stdObservers: StdObservers)
    (implicit scheduler: Scheduler, iox: IOExecutor): ShellScriptProcess =
  {
    val processBuilder = new ProcessBuilder(toShellCommandArguments(
      commandLine.file, commandLine.arguments.tail ++ conf.idArgumentOption/*TODO Should not be an argument*/).asJava)
    for (o <- conf.workingDirectory) processBuilder.directory(o.toFile)
    processBuilder.environment.putAll(conf.additionalEnvironment.asJava)
    val process = processBuilder.startRobustly()

    def toObservable(in: InputStream, onTerminated: Promise[Unit]): Observable[String] =
      UnbufferedReaderObservable(Task(new InputStreamReader(in)), stdObservers.charBufferSize)
        .executeOn(iox.scheduler)
        .guaranteeCase {
          case ExitCase.Error(t) => Task(onTerminated.failure(t))
          case ExitCase.Completed | ExitCase.Canceled => Task(onTerminated.success(()))
        }

    val outClosed, errClosed = Promise[Unit]()
    toObservable(process.getInputStream, outClosed).subscribe(stdObservers.out)
    toObservable(process.getErrorStream, errClosed).subscribe(stdObservers.err)

    new ShellScriptProcess(conf, process, argumentsForLogging = commandLine.toString :: Nil) {
      override def terminated = for {
        outTried <- outClosed.future transformWith Future.successful
        errTried <- errClosed.future transformWith Future.successful
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
}
