package js7.executor.process

import cats.effect.ExitCase
import java.io.{InputStream, InputStreamReader}
import java.lang.ProcessBuilder.Redirect.PIPE
import js7.base.io.process.Processes._
import js7.base.io.process.{JavaProcess, Js7Process}
import js7.base.log.Logger
import js7.base.monixutils.UnbufferedReaderObservable
import js7.base.problem.Checked
import js7.base.system.OperatingSystem.isUnix
import js7.base.thread.IOExecutor
import js7.base.utils.ScalaUtils.syntax._
import js7.data.job.CommandLine
import js7.executor.StdObservers
import js7.executor.forwindows.WindowsProcess
import js7.executor.forwindows.WindowsProcess.StartWindowsProcess
import monix.eval.Task
import monix.reactive.Observable
import scala.concurrent.{ExecutionContext, Promise}
import scala.jdk.CollectionConverters._

class ShellScriptProcess private(
  processConfiguration: ProcessConfiguration,
  process: Js7Process)
  (implicit ec: ExecutionContext, iox: IOExecutor)
extends RichProcess(processConfiguration, process)
{
  stdin.close() // Process gets an empty stdin
}

object ShellScriptProcess
{
  private val logger = Logger(getClass)

  def startPipedShellScript(
    commandLine: CommandLine,
    conf: ProcessConfiguration,
    stdObservers: StdObservers,
    whenTerminated: Task[Unit] = Task.unit)
    (implicit iox: IOExecutor)
  : Task[Checked[ShellScriptProcess]] =
    Task.deferAction { implicit scheduler =>
      val commandArgs = toShellCommandArguments(
        commandLine.file,
        commandLine.arguments.tail ++ conf.idArgumentOption/*TODO Should not be an argument*/)
      // Check argsToCommandLine here to avoid exception in WindowsProcess.start

      startProcess(commandArgs, conf)
        .map(_.map { process =>
          def toObservable(in: InputStream, onTerminated: Promise[Unit]): Observable[String] =
            UnbufferedReaderObservable(Task(new InputStreamReader(in)), stdObservers.charBufferSize)
              .executeOn(iox.scheduler)
              .guaranteeCase {
                case ExitCase.Error(t) => Task(onTerminated.failure(t))
                case ExitCase.Completed | ExitCase.Canceled => Task(onTerminated.success(()))
              }

          val outClosed, errClosed = Promise[Unit]()
          toObservable(process.stdout, outClosed).subscribe(stdObservers.out)
          toObservable(process.stderr, errClosed).subscribe(stdObservers.err)

          new ShellScriptProcess(conf, process) {
            override val terminated =
              ( for {
                 outTried <- Task.fromFuture(outClosed.future).materialize
                 errTried <- Task.fromFuture(errClosed.future).materialize
                 returnCode <- super.terminated
                 returnCode <-
                   if (isKilled || isUnix && returnCode.isProcessSignal/*externally killed?*/) {
                     for (t <- outTried.failed) logger.warn(t.toStringWithCauses)
                     for (t <- errTried.failed) logger.warn(t.toStringWithCauses)
                     Task.pure(returnCode)
                   } else
                     Task.fromTry(for (_ <- outTried; _ <- errTried) yield returnCode)
                 _ <- whenTerminated
                } yield returnCode
              ).memoize
          }
        })
    }

  private def startProcess(args: Seq[String], conf: ProcessConfiguration)
  : Task[Checked[Js7Process]] =
    conf.windowsLogon match {
      case None =>
        val processBuilder = new ProcessBuilder(args.asJava)
        for (o <- conf.workingDirectory) processBuilder.directory(o.toFile)
        processBuilder.environment.putAll(conf.additionalEnvironment.asJava)
        processBuilder.startRobustly()
          .map(o => Right(JavaProcess(o)))

      case Some(logon) =>
        Task(
          WindowsProcess.startWithWindowsLogon(
            StartWindowsProcess(
              args,
              stdinRedirect = PIPE,
              stdoutRedirect = PIPE,
              stderrRedirect = PIPE,
              additionalEnv = conf.additionalEnvironment),
            Some(logon)))

  }
}
