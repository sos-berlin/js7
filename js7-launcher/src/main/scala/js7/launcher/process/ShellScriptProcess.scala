package js7.launcher.process

import cats.effect.{Deferred, FiberIO, IO, Outcome}
import java.io.{IOException, InputStream}
import java.lang.ProcessBuilder.Redirect.PIPE
import js7.base.catsutils.CatsEffectExtensions.{joinStd, raceBoth, startAndForget}
import js7.base.catsutils.UnsafeMemoizable.unsafeMemoize
import js7.base.io.process.Processes.*
import js7.base.io.process.{JavaProcess, Js7Process, ReturnCode, Stderr, Stdout, StdoutOrStderr}
import js7.base.log.Logger
import js7.base.log.Logger.syntax.*
import js7.base.problem.Checked
import js7.base.thread.IOExecutor
import js7.base.time.ScalaTime.*
import js7.base.utils.Atomic.extensions.*
import js7.base.utils.CatsUtils.syntax.logWhenItTakesLonger
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.utils.Worry.AfterTenSecondsWorryDurations
import js7.base.utils.{Atomic, Worry}
import js7.data.job.{CommandLine, JobKey}
import js7.data.order.OrderId
import js7.launcher.StdObservers
import js7.launcher.forwindows.WindowsProcess
import js7.launcher.forwindows.WindowsProcess.StartWindowsProcess
import scala.annotation.unused
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

  def watchProcessAndStdouterr: IO[ReturnCode]

  protected def afterSigkill =
    sigkilled.complete(()).void

  protected def afterSigterm =
    sigtermed.complete(()).void


object ShellScriptProcess:
  private val logger = Logger[this.type]
  /** Grace period between SIGKILL and (second) destroyForcibly. */
  private val killStdoutAndStderrDelay = 500.ms
  private val stdoutAndStderrAbandonAfter = 3.s
  private val StdouterrWorryStart = 100.ms
  private val StdouterrWorry = Worry(
    Seq(1.s, 3.s, 6.s) ++ AfterTenSecondsWorryDurations,
    infoLevel = 0.s, orangeLevel = 3.s)

  private val pumpFiberCount = Atomic(0)

  def startPipedShellScript(
    commandLine: CommandLine,
    conf: ProcessConfiguration,
    stdObservers: StdObservers,
    orderId: OrderId,
    jobKey: JobKey)
    (using IOExecutor)
  : IO[Checked[ShellScriptProcess]] =
    IO.defer:
      val commandArgs = toShellCommandArguments(
        commandLine.file,
        commandLine.arguments.tail ++ conf.idArgumentOption /*TODO Should not be an argument*/)
      // Check argsToCommandLine here to avoid exception in WindowsProcess.start

      startProcess(commandArgs, conf)
        .map { _.map: process =>
          new ShellScriptProcess(conf, process):
            val watchProcessAndStdouterr = watchProcess2.unsafeMemoize

            private def watchProcess2: IO[ReturnCode] =
              IO.raceBoth(
                  awaitProcessTermination,
                  IO.raceBoth(
                      sigkilled.get.andWait(killStdoutAndStderrDelay),
                      pumpStdoutAndStderrToSink)
                    .flatMap:
                      case Left((), awaitStdouterr) =>
                        IO.defer:
                          logger.warn:
                            s"$orderId $toString ignoring stdout and stderr after SIGKILL (maybe a child proces is still running)"
                          joinStdouterrInBackground(
                            awaitStdouterr, orderId, jobKey, stdoutAndStderrAbandonAfter)
                        .as(false/*stdout ignored*/)
                      case Right((awaitSigkill, ())) =>
                        awaitSigkill.cancel
                        .as(true/*stdout ended*/))
                .flatMap:
                  case Left((returnCode, awaitStdouterr)) => // Process terminated
                    IO.defer:
                      logger.debug(s"$orderId $toString terminated with $returnCode")
                      IO:
                        logger.info:
                          s"$orderId $toString terminated with $returnCode, awaiting stdout or stderr (maybe a child proces is still running)"
                      .delayBy(StdouterrWorryStart)
                      .background.surround:
                        val what = s"$orderId stdout or stderr"
                        awaitStdouterr.joinStd
                          .logWhenItTakesLonger(StdouterrWorry):
                            case (None, elapsed, level, sym) => IO.pure:
                              s"$sym Still waiting for $what for ${elapsed.pretty}"
                            case (Some(Outcome.Succeeded(ended)), elapsed, level, _) =>
                              ended.flatMap(ended => IO:
                                if ended
                                then s"🔵 $what ended after ${elapsed.pretty}"
                                else s"🟣 $what ignored after ${elapsed.pretty}")
                            case (Some(Outcome.Canceled()), elapsed, level, sym) => IO.pure:
                              s"$sym $what canceled after ${elapsed.pretty}"
                            case (Some(Outcome.Errored(t)), elapsed, level, sym) => IO.pure:
                              s"$sym $what failed after ${elapsed.pretty} with ${t.toStringWithCauses}"
                    .as(returnCode)

                  case Right((awaitTermination, _)) =>
                    // Stdout and stderr ended or ignored after SIGKILL
                    awaitTermination.joinStd

            @unused private def onSigkilledCloseStdouterr: IO[Unit] =
              // The process may have terminated while long running child processes have inherited
              // the file handles and still use them.
              // After SIGKILL, we forcibly close stdout and stderr with destroyForcibly.
              // We delay a short while to allow the last outstanding data to be handled by
              // pumpStdoutAndStderrToSink.
              // Still running child processes write operations will fail with EPIPE or may block !!!
              sigkilled.get
                .andWait(killStdoutAndStderrDelay)
                .flatMap(_ => IO:
                  logger.info(s"$orderId destroyForcibly to close stdout and stderr")
                  process.destroyForcibly())

            private def pumpStdoutAndStderrToSink: IO[Unit] =
              logger.traceIO(s"$orderId pumpStdoutAndStderrToSink")(IO
                .both(
                  pumpOutErrToSink(Stdout, process.stdout),
                  pumpOutErrToSink(Stderr, process.stderr))
                .void)

            private def pumpOutErrToSink(outErr: StdoutOrStderr, in: InputStream): IO[Unit] =
              stdObservers
                .pumpInputStreamToSink(outErr, in, conf.encoding)
                .handleErrorWith:
                  case t: IOException if isKilling /*Happens under Windows*/ => IO:
                    logger.warn(
                      s"$orderId: While killing the process, $outErr became unreadable: ${t.toStringWithCauses}",
                      t)
                  case t => IO(logger.warn(s"$orderId $outErr: ${t.toStringWithCauses}"))
        }

  private def joinStdouterrInBackground(
    fiber: FiberIO[Unit],
    orderId: OrderId,
    jobKey: JobKey,
    timeout: FiniteDuration)
  : IO[Unit] =
    IO.defer:
      val since = Deadline.now
      var logged = false
      import ShellScriptProcess.pumpFiberCount as n
      n += 1
      IO
        .raceBoth(
          fiber.joinStd
            .*>(IO:
              if logged then
                // Not reliable due to race condition
                logger.info:
                  s"🟣 $orderId $jobKey: ignored stdout and stderr have finally ended after ${
                    since.elapsed.pretty}${(n.get > 1) ?? s" (n=$n)"}")
            .guarantee:
              IO(n -= 1).void,
          IO.defer:
            IO.sleep(timeout) *> IO:
              // Order has proceeded by now
              logged = true
              logger.info:
                s"🟣 $orderId $jobKey: still ignoring stdout and stderr after process termination ${
                  since.elapsed.pretty} ago${(n.get > 1) ?? s" (n=$n)"}")
        .flatMap:
          case Left(((), logging)) => logging.cancel
          case Right((_, ())) => IO.unit // Forget fiber
        .startAndForget

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
