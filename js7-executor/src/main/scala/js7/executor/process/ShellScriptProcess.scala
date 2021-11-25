package js7.executor.process

import java.lang.ProcessBuilder.Redirect.PIPE
import js7.base.io.process.Processes._
import js7.base.io.process.{JavaProcess, Js7Process}
import js7.base.log.Logger
import js7.base.problem.Checked
import js7.base.system.OperatingSystem.isUnix
import js7.base.thread.IOExecutor
import js7.base.utils.ScalaUtils.syntax._
import js7.data.job.CommandLine
import js7.executor.StdObservers
import js7.executor.forwindows.WindowsProcess
import js7.executor.forwindows.WindowsProcess.StartWindowsProcess
import js7.executor.process.InputStreamToObservable.copyInputStreamToObservable
import monix.eval.Task
import scala.jdk.CollectionConverters._

class ShellScriptProcess private(
  processConfiguration: ProcessConfiguration,
  process: Js7Process)
  (implicit iox: IOExecutor)
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
          new ShellScriptProcess(conf, process) {
            import stdObservers.{charBufferSize, err, out}
            override val terminated =
              (for {
                outFiber <- copyInputStreamToObservable(process.stdout, out, charBufferSize)
                errFiber <- copyInputStreamToObservable(process.stderr, err, charBufferSize)
                outTried <- outFiber.join.materialize
                errTried <- errFiber.join.materialize
                returnCode <- super.terminated
                returnCode <-
                  if (isKilled || isUnix && returnCode.isProcessSignal/*externally killed?*/) {
                    for (t <- outTried.failed) logger.warn(t.toStringWithCauses)
                    for (t <- errTried.failed) logger.warn(t.toStringWithCauses)
                    Task.pure(returnCode)
                  } else
                    Task.fromTry(for (_ <- outTried; _ <- errTried) yield returnCode)
                _ <- whenTerminated
              } yield returnCode).memoize
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
