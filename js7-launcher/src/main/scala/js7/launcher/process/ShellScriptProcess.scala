package js7.launcher.process

import cats.effect.{Deferred, FiberIO, IO}
import java.io.{IOException, InputStream}
import java.lang.ProcessBuilder.Redirect.PIPE
import js7.base.catsutils.CatsEffectExtensions.{joinStd, startAndForget}
import js7.base.catsutils.UnsafeMemoizable.unsafeMemoize
import js7.base.io.process.Processes.*
import js7.base.io.process.{JavaProcess, Js7Process, ReturnCode, Stderr, Stdout, StdoutOrStderr}
import js7.base.log.Logger
import js7.base.log.Logger.syntax.*
import js7.base.problem.Checked
import js7.base.thread.IOExecutor
import js7.base.time.ScalaTime.*
import js7.base.utils.Atomic.extensions.*
import js7.base.utils.CatsUtils.TenSecondsWorryDurations
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.utils.{Atomic, CatsUtils}
import js7.data.job.CommandLine
import js7.launcher.StdObservers
import js7.launcher.forwindows.WindowsProcess
import js7.launcher.forwindows.WindowsProcess.StartWindowsProcess
import scala.concurrent.duration.{Deadline, FiniteDuration}
import scala.jdk.CollectionConverters.*

abstract class ShellScriptProcess private(
  processConfiguration: ProcessConfiguration,
  process: Js7Process)
  (implicit iox: IOExecutor)
extends RichProcess(processConfiguration, process):

  stdin.close() // Process gets an empty stdin

  private[ShellScriptProcess] val sigkilled = Deferred.unsafe[IO, Unit]
  private[ShellScriptProcess] val sigtermed = Deferred.unsafe[IO, Unit]

  def watchProcess: IO[ReturnCode]

  protected def onSigkill =
    sigkilled.complete(()).void

  protected def onSigterm =
    sigtermed.complete(()).void


object ShellScriptProcess:
  private val logger = Logger[this.type]
  /** Grace period between SIGKILL and (second) destroyForcibly. */
  private val stdoutAndStderrDetachDelay = 500.ms
  private val pumpFiberCount = Atomic(0)
  private val TerminationWorryDurations: Seq[FiniteDuration] =
    Seq(1.s, 2.s, 7.s) ++ TenSecondsWorryDurations

  def startPipedShellScript(
    commandLine: CommandLine,
    conf: ProcessConfiguration,
    stdObservers: StdObservers,
    orderId: OrderId,
    jobKey: JobKey)
    (using IOExecutor)
  : IO[Checked[ShellScriptProcess]] =
    val name = s"$orderId $jobKey"
    IO.defer:
      val commandArgs = toShellCommandArguments(
        commandLine.file,
        commandLine.arguments.tail ++ conf.idArgumentOption /*TODO Should not be an argument*/)
      // Check argsToCommandLine here to avoid exception in WindowsProcess.start

      startProcess(commandArgs, conf)
        .map(_.map: process =>
          new ShellScriptProcess(conf, process):
            val watchProcess = watchProcess2.unsafeMemoize

            override def awaitProcessTermination = watchProcess

            private def pumpOutErrToSink(outErr: StdoutOrStderr, in: InputStream): IO[Unit] =
              stdObservers
                .pumpInputStreamToSink(outErr, in, conf.encoding)
                .handleErrorWith:
                  case t: IOException if isKilling /*Happens under Windows*/ => IO:
                    logger.warn(
                      s"$name: While killing the process, $outErr become unreadable: ${t.toStringWithCauses}",
                      t)
                  case t => IO:
                    logger.warn(s"$name $outErr: ${t.toStringWithCauses}")

            private def pumpBothStdoutAndStderrToSink: IO[Unit] =
              logger.traceIO("pumpBothStdoutAndStderrToSink", name)(IO
                .both(
                  pumpOutErrToSink(Stdout, process.stdout),
                  pumpOutErrToSink(Stderr, process.stderr))
                .void)

            private def watchProcess2: IO[ReturnCode] =
              for
                _ <-
                  IO
                    .racePair(
                      // Sometimes, the process is not really killed.
                      // Then we destroyForcibly. Because this closes stdout and stderr, we
                      // wait a short while to allow the last outstanding data to handled by
                      // the InputStream pump. See also RichProcess superclass.
                      sigkilled.get
                        .andWait(stdoutAndStderrDetachDelay)
                        .map: _ =>
                          //IO.whenA(process.isAlive):
                          IO(logger.debug(s"$name destroyForcibly $process")) *>
                            IO.blocking(process.destroyForcibly()),
                      pumpBothStdoutAndStderrToSink)
                    .flatMap:
                      case Left((_, pumpFiber)) => joinPumpFiber(pumpFiber, name).startAndForget
                      case Right((whenSigkilledFiber, _)) => whenSigkilledFiber.cancel
                returnCode <- super.awaitProcessTermination
              yield returnCode)

  private def joinPumpFiber(fiber: FiberIO[Unit], name: String): IO[Unit] =
    IO.defer:
      val since = Deadline.now
      var logged = false
      import ShellScriptProcess.pumpFiberCount as n
      n += 1
      IO
        .both(
          fiber.joinStd
            .*>(IO:
              if logged then
                // Not reliable due to race condition
                logger.info:
                  s"⚪️ $name stdout and stderr have finally ended after ${
                    since.elapsed.pretty}${(n.get > 0) ?? s" (n=$n)"}")
            .guarantee:
              IO(n -= 1).void,
          IO.defer:
            IO.sleep(3.s) *> IO:
              logged = true
              logger.warn:
                s"🟤 $name still reading stdout or stderr after process termination ${
                  since.elapsed.pretty} ago${
                  (n.get > 1) ?? s" (n=$n)"}")
        .void

  private def startProcess(args: Seq[String], conf: ProcessConfiguration)
  : IO[Checked[Js7Process]] =
    conf.windowsLogon match
      case None =>
        val processBuilder = new ProcessBuilder(args.asJava)
        for o <- conf.workingDirectory do processBuilder.directory(o.toFile)

        transferEnv(from = conf.additionalEnvironment, to = processBuilder.environment)

        processBuilder.startRobustly()
          .map(o => Right(JavaProcess(o)))

      case Some(logon) =>
        IO(
          WindowsProcess.startWithWindowsLogon(
            StartWindowsProcess(
              args,
              stdinRedirect = PIPE,
              stdoutRedirect = PIPE,
              stderrRedirect = PIPE,
              additionalEnv = conf.additionalEnvironment),
            Some(logon)))

  private def transferEnv(
    from: Map[String, Option[String]],
    to: java.util.Map[String, String])
  : Unit =
    from
      .collect { case (k, None) => k }
      .foreach(to.remove)

    to.putAll(from
      .collect { case (k, Some(v)) => k -> v }
      .asJava)
