package js7.launcher.process

import cats.effect.{Deferred, FiberIO, IO, Outcome}
import cats.syntax.foldable.*
import cats.syntax.traverse.*
import java.io.{IOException, InputStream, OutputStream}
import java.lang.ProcessBuilder.Redirect
import java.lang.ProcessBuilder.Redirect.PIPE
import js7.base.catsutils.CatsEffectExtensions.{joinStd, raceBoth, right, startAndForget}
import js7.base.catsutils.UnsafeMemoizable.memoize
import js7.base.io.process.ProcessSignal.SIGKILL
import js7.base.io.process.Processes.*
import js7.base.io.process.{JavaProcess, Js7Process, Pid, ProcessSignal, ReturnCode, Stderr, Stdout, StdoutOrStderr}
import js7.base.log.Logger
import js7.base.log.Logger.syntax.*
import js7.base.problem.Checked
import js7.base.system.OperatingSystem
import js7.base.system.OperatingSystem.isWindows
import js7.base.thread.IOExecutor
import js7.base.thread.IOExecutor.env.interruptibleVirtualThread
import js7.base.time.ScalaTime.*
import js7.base.utils.Atomic.extensions.*
import js7.base.utils.CatsUtils.syntax.*
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.utils.Worry.AfterTenSecondsWorryDurations
import js7.base.utils.{Atomic, Worry}
import js7.data.job.{CommandLine, JobKey}
import js7.data.order.OrderId
import js7.launcher.StdObservers
import js7.launcher.forwindows.WindowsProcess
import js7.launcher.forwindows.WindowsProcess.StartWindowsProcess
import js7.launcher.process.PipedProcess.*
import org.jetbrains.annotations.TestOnly
import scala.concurrent.duration.Deadline.now
import scala.concurrent.duration.{Deadline, FiniteDuration}
import scala.jdk.CollectionConverters.*
import scala.jdk.OptionConverters.*

final class PipedProcess private(
  val conf: ProcessConfiguration,
  process: Js7Process,
  stdObservers: StdObservers,
  orderId: OrderId,
  jobKey: JobKey):

  private val logger = Logger.withPrefix[this.type](toString)
  private val sigkilled = Deferred.unsafe[IO, Unit]
  @volatile private var _isKilling = false

  private val runningSince = now

  def duration: FiniteDuration = runningSince.elapsed

  private val _awaitProcessTermination: IO[ReturnCode] =
    memoize:
      IO.defer:
        process.returnCode.map(IO.pure)
          .getOrElse:
            waitForProcessTermination(process)

  val watchProcessAndStdouterr: IO[ReturnCode] =
    memoize:
      IO.raceBoth(
          _awaitProcessTermination,
          IO.raceBoth(
              sigkilled.get.andWait(killStdoutAndStderrDelay),
              pumpStdoutAndStderrToSink)
            .flatMap:
              case Left((), stdouterrFiber) =>
                IO.defer:
                  logger.warn:
                    s"Ignoring stdout and stderr after SIGKILL (maybe a child proces is still running)"
                  joinStdouterr(stdouterrFiber, orderId, jobKey, stdoutAndStderrAbandonAfter)
                    .startAndForget
                .as(false /*stdout ignored*/)
              case Right((sigkilledFiber, ())) =>
                sigkilledFiber.cancel
                  .as(true /*stdout ended*/))
        .flatMap:
          case Left((returnCode, stdouterrFiber)) => // Process terminated
            IO.defer:
              logger.debug(s"Tterminated with $returnCode")
              IO:
                logger.info:
                  s"terminated with $returnCode, awaiting stdout or stderr (maybe a child proces is still running)"
              .delayBy(StdouterrWorryStart)
              .background.surround:
                val what = s"$orderId stdout or stderr"
                stdouterrFiber.joinStd
                  .logWhenItTakesLonger(StdouterrWorry):
                    case (None, elapsed, level, sym) => IO.pure:
                      s"$sym Still waiting for $what for ${elapsed.pretty}"
                    case (Some(Outcome.Succeeded(ended)), elapsed, level, _) =>
                      ended.flatMap(ended => IO:
                        if ended
                        then s"🔵 $what ended after ${elapsed.pretty}"
                        else s"🟣 $what are ignored after ${elapsed.pretty}")
                    case (Some(Outcome.Canceled()), elapsed, level, sym) => IO.pure:
                      s"$sym $what canceled after ${elapsed.pretty}"
                    case (Some(Outcome.Errored(t)), elapsed, level, sym) => IO.pure:
                      s"$sym $what failed after ${elapsed.pretty} with ${t.toStringWithCauses}"
            .as(returnCode)

          case Right((terminationFiber, _)) =>
            // Stdout and stderr ended or ignored after SIGKILL
            terminationFiber.joinStd

  private def onSigkilledCloseStdouterr: IO[Unit] =
    // The process may have terminated while long running child processes have inherited
    // the file handles and still use them.
    // After SIGKILL, we forcibly close stdout and stderr with destroyForcibly.
    // We delay a short while to allow the last outstanding data to be handled by
    // pumpStdoutAndStderrToSink.
    // Still running child processes write operations will fail with EPIPE or may block !!!
    sigkilled.get
      .andWait(killStdoutAndStderrDelay)
      .flatMap(_ => IO:
        logger.info(s"destroyForcibly to close stdout and stderr")
        process.destroyForcibly())

  private def pumpStdoutAndStderrToSink: IO[Unit] =
    logger.traceIO(s"pumpStdoutAndStderrToSink")(IO
      .both(
        pumpOutErrToSink(Stdout, process.stdout),
        pumpOutErrToSink(Stderr, process.stderr))
      .void)

  private def pumpOutErrToSink(outErr: StdoutOrStderr, in: InputStream): IO[Unit] =
    stdObservers
      .pumpInputStreamToSink(outErr, in, conf.encoding)
      .handleErrorWith:
        case t: IOException if isWindows && _isKilling => IO:
          logger.warn(
            s"While killing the process, $outErr became unreadable: ${t.toStringWithCauses}",
            t)
        case t => IO(logger.warn(s"$outErr: ${t.toStringWithCauses}"))

  def sendProcessSignal(signal: ProcessSignal): IO[Unit] =
    IO.defer:
      val force = signal == SIGKILL
      _isKilling |= force
      kill(force)
        .guarantee:
          IO.whenA(force):
            sigkilled.complete(()).void

  private def kill(force: Boolean): IO[Unit] =
    if !force then
      killMainProcessOnly(force)
    else
      killWithDescendants(force)

  private def killWithDescendants(force: Boolean): IO[Unit] =
    logger.debugIO:
      IO.defer:
        val descendants = process.descendants
        if descendants.isEmpty then
          killMainProcessOnly(force)
        else
          // Try to stop all processes, then collect descendants again and kill them
          sendStopSignal(process.pid.number +: descendants.map(_.pid))
            .handleErrorWith: t =>
              IO(logger.warn(t.toStringWithCauses))
            .flatMap: _ =>
              val descendants = process.descendants
              killMainProcessOnly(force).flatMap: _ =>
                descendants
                  .traverse(killViaProcessHandle(_, force))
                  .map(_.combineAll)

  private def sendStopSignal(pids: Seq[Long]): IO[Unit] =
    IO.defer:
      val firstArgs = Vector("/bin/kill", "-STOP")
      val args = firstArgs ++ pids.map(_.toString)
      logger.info(s"${args.mkString(" ")}")
      runProcess(args): line =>
        IO(logger.warn(s"${firstArgs.mkString(" ")}: $line"))
      .flatMap(returnCode => IO:
        if !returnCode.isSuccess then
          logger.warn(s"${firstArgs.mkString(" ")} exited with $returnCode"))

  private def killMainProcessOnly(force: Boolean): IO[Unit] =
    IO.defer:
      process.match
        case process: JavaProcess =>
          // Kill via ProcessHandle because this doesn't close stdout and stdderr
          killViaProcessHandle(process.handle, force)
        case _ =>
          killWithJava(force)

  private def killViaProcessHandle(processHandle: ProcessHandle, force: Boolean, indent: Int = 0)
  : IO[Unit] =
    IO:
      logger.info:
        val isMainProcesss = processHandle.pid == process.pid.number
        (if isMainProcesss then "⚫️ " else "❌ ") +
          (if force then """destroyForcibly "SIGKILL" """ else """destroy "SIGTERM" """) +
          (!isMainProcesss ?? (
            " " * (indent - 1) +
              "child process " +
              Pid(processHandle.pid) +
              ' ' +
              processHandle.info.commandLine.toScala.getOrElse("")))
      if force then
        processHandle.destroyForcibly()
      else
        processHandle.destroy()

  private def killWithJava(force: Boolean): IO[Unit] =
    IO:
      if force then
        logger.info("⚫️ destroyForcibly (SIGKILL)")
        process.destroyForcibly()
      else
        logger.info("⚫️ destroy (SIGTERM)")
        process.destroy()

  @TestOnly
  private[process] def isAlive = process.isAlive

  private def waitForProcessTermination(process: Js7Process): IO[ReturnCode] =
    interruptibleVirtualThread:
      logger.traceCallWithResult(s"waitFor $process"):
        process.waitFor()

  def stdin: OutputStream =
    process.stdin

  override def toString =
    s"$orderId $process"


object PipedProcess:

  private val logger = Logger[this.type]
  /** Grace period between SIGKILL and (second) destroyForcibly. */
  private val killStdoutAndStderrDelay = 500.ms
  private val stdoutAndStderrAbandonAfter = 3.s
  private val StdouterrWorryStart = 100.ms
  private val StdouterrWorry = Worry(
    Seq(1.s, 3.s, 6.s) ++ AfterTenSecondsWorryDurations,
    infoLevel = 0.s, orangeLevel = 3.s)

  private val pumpFiberCount = Atomic(0)

  /** Start a process and read stdout and stderr as pipes. */
  def start(
    commandLine: CommandLine,
    conf: ProcessConfiguration,
    stdObservers: StdObservers,
    orderId: OrderId,
    jobKey: JobKey)
  : IO[Checked[PipedProcess]] =
    IO.defer:
      val commandArgs = toShellCommandArguments(commandLine.file, commandLine.arguments.tail)
      // Check argsToCommandLine here to avoid exception in WindowsProcess.start

      startProcess(commandArgs, conf).flatMapT: process =>
        IO.right:
          process.stdin.close() // Process gets an empty stdin
          new PipedProcess(conf, process, stdObservers, orderId, jobKey)

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
        IO:
          WindowsProcess.startWithWindowsLogon(
            StartWindowsProcess(
              args,
              stdinRedirect = PIPE,
              stdoutRedirect = PIPE,
              stderrRedirect = PIPE,
              additionalEnv = conf.additionalEnvironment),
            Some(logon))

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

  private def joinStdouterr(
    fiber: FiberIO[Unit],
    orderId: OrderId,
    jobKey: JobKey,
    timeout: FiniteDuration)
  : IO[Unit] =
    IO.defer:
      val since = Deadline.now
      var logged = false
      import pumpFiberCount as n
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
