package js7.launcher.process

import cats.effect.{FiberIO, IO, Outcome}
import cats.syntax.option.*
import fs2.concurrent.SignallingRef
import java.io.{IOException, InputStream}
import java.lang.ProcessBuilder.Redirect.PIPE
import js7.base.catsutils.CatsEffectExtensions.{fromOutcome, joinStd, raceBoth, raceMerge, startAndForget}
import js7.base.catsutils.UnsafeMemoizable.memoize
import js7.base.io.process.ProcessExtensions.onExitIO
import js7.base.io.process.ProcessSignal.SIGKILL
import js7.base.io.process.Processes.*
import js7.base.io.process.StartRobustly.startRobustly
import js7.base.io.process.{JavaProcess, Js7Process, Pid, ProcessSignal, ReturnCode, Stderr, Stdout, StdoutOrStderr}
import js7.base.log.Logger
import js7.base.log.Logger.syntax.*
import js7.base.problem.Checked
import js7.base.system.OperatingSystem.isWindows
import js7.base.thread.IOExecutor.env.interruptibleVirtualThread
import js7.base.time.ScalaTime.*
import js7.base.utils.Atomic.extensions.*
import js7.base.utils.CatsUtils.syntax.*
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.utils.Worry.AfterTenSecondsWorryDurations
import js7.base.utils.{Allocated, Atomic, Worry}
import js7.data.job.{CommandLine, JobKey}
import js7.data.order.OrderId
import js7.launcher.StdObservers
import js7.launcher.forwindows.WindowsProcess
import js7.launcher.forwindows.WindowsProcess.StartWindowsProcess
import js7.launcher.process.PipedProcess.*
import js7.launcher.processkiller.SubagentProcessKiller
import org.jetbrains.annotations.TestOnly
import scala.concurrent.duration.{Deadline, FiniteDuration}
import scala.jdk.CollectionConverters.*
import scala.util.chaining.scalaUtilChainingOps

final class PipedProcess private(
  val conf: ProcessConfiguration,
  private[process] val process: Js7Process,
  stdObservers: StdObservers,
  orderId: OrderId,
  jobKey: JobKey,
  killSignal: SignallingRef[IO, Option[ProcessSignal]],
  processKillerAlloc: Allocated[IO, SubagentProcessKiller],
  runningSince: Deadline):

  import stdObservers.maxWaitForStdouterr

  private val processKiller = processKillerAlloc.allocatedThing
  private var _processTerminated = false
  private def label = s"$orderId $process"
  private val logger = Logger.withLivePrefix[this.type](label)

  def pid: Pid =
    process.pid

  def pidString: String =
    pid.toString + (_processTerminated ?? "†")

  def duration: FiniteDuration =
    runningSince.elapsed

  def release: IO[Unit] =
    processKillerAlloc.release

  val awaitProcessTermination: IO[ReturnCode] =
    memoize:
      process.maybeHandle.fold(IO.unit)(_.onExitIO) *>
        IO.defer:
          process.returnCode.map(IO.pure)
            .getOrElse:
              interruptibleVirtualThread:
                logger.traceCallWithResult(s"waitFor $process"):
                  process.waitFor()
        .flatTap: rc =>
          IO:
            ProcessMXBean.running -= 1
            _processTerminated = true
            logger.trace(s"Process terminated with $rc after ${duration.pretty}")

  /** A JS7 process completes when
    * - The process has terminated, and
    * - Reading stdout and stderr has terminated, which means:
    *   - stdout and stderr has been read until EOF, or
    *   - the process has been SIGKILLed and StdouterrAbandonAfter has elapsed, or
    *   - the process has not been SIGKILLed and maxWaitForStdouterr has elapsed.
    *
    * If reading of stdout/stderr is terminated before reaching EOF, then this may happen:
    * - Reading stdout/stderr may block in InputStream.
    * - Then, cancellation only takes effect when InputStream returns the next chunk or EOF.
    * - And Fiber#cancel blocks, too.
    * - We must continue the reading stdout/stderr background.
    * - The reading background stdouterrFiber must not emit an OrderStdWritten event.
    */
  val waitForEndOfProcessAndStdouterr: IO[ReturnCode] =
    memoize:
      pumpStdouterrToSink
        .raceBoth:
          awaitProcessTermination
        .flatMap:
          case Left(((), processFiber)) =>
            // Stdout and stderr ended properly, wait for process termination
            processFiber.joinStd

          case Right((stdouterrFiber, returnCode)) =>
            logger.debug(s"Process terminated with $returnCode, waiting for stdout or stderr")
            waitForStdouterr(stdouterrFiber, returnCode).as(returnCode)
        .guarantee:
          process.release

  private def pumpStdouterrToSink: IO[Unit] =
    logger.traceIO(s"pumpStdouterrToSink"):
      // If one channel fails, continue with the other channel
      pumpOutErrToSink(Stdout, process.stdout).bothOutcome:
        pumpOutErrToSink(Stderr, process.stderr)
      .flatMap:
        case (Outcome.Succeeded(_), stderrOutcome) =>
          IO.fromOutcome(stderrOutcome)
        case (stdoutFailed, stderrOutcome) =>
          if !stderrOutcome.isSuccess then logger.error(s"While reading stderr: $stderrOutcome")
          IO.fromOutcome(stdoutFailed)
      .logWhenItTakesLonger(StdouterrWorry):
        case (outcome, elapsed, _, sym) =>
          stdObservers.stdouterrStopper.peek.flatMap: stopped =>
            val what = s"${stopped ?? "Ignored "}stdout and stderr of $orderId $pidString"
            outcome match
              case None =>
                IO.pure(s"$sym Still waiting for $what for ${elapsed.pretty}")
              case Some(Outcome.Succeeded(_) | Outcome.Canceled()) =>
                IO.pure:
                  if stopped then
                    s"🟣 $what finally closed after ${elapsed.pretty}"
                  else
                    s"$sym $what finally ended after ${elapsed.pretty}"
              case Some(Outcome.Errored(t)) =>
                IO.pure:
                  s"$sym $what failed after ${elapsed.pretty} with ${t.toStringWithCauses}"

  private def pumpOutErrToSink(outErr: StdoutOrStderr, in: InputStream): IO[Unit] =
    stdObservers
      .pumpInputStreamToSink(outErr, in, conf.encoding)
      .handleErrorWith: t =>
        killSignal.get.map(_.isDefined).map: isKilling =>
          t match
            case t: IOException if isWindows && isKilling =>
              logger.warn:
                s"While killing the process, $outErr became unreadable: ${t.toStringWithCauses}"
            case t => logger.warn(s"$outErr: ${t.toStringWithCauses}")

  private def waitForStdouterr(stdouterrFiber: FiberIO[Unit], returnCode: ReturnCode): IO[Unit] =
    killSignal.get.map(_.contains(SIGKILL)).flatMap: wasSigkilled =>
      IO: // delayed!
        logger.info(s"Process terminated with $returnCode, waiting for stdout and stderr${
          maxWaitForStdouterr.fold(""): o =>
            s" for maxWaitForStdouterr=${o.pretty}"}")
      .delayBy(conf.worryAboutStdoutAfterTermination)
      .background.surround:
        stdouterrFiber.joinWithUnit.as("")
          .pipe:
            if wasSigkilled then
              _.timeoutTo(
                conf.waitForStdouterrAfterSigkill,
                IO(s" ${conf.waitForStdouterrAfterSigkill.pretty} after SIGKILL"))
            else
              maxWaitForStdouterr.fold_(identity, maxWaitForStdouterr =>
                _.timeoutTo(
                  maxWaitForStdouterr,
                  IO(s" because maxWaitForStdouterr=${maxWaitForStdouterr.pretty} has elapsed")))
          .raceMerge:
            killSignal.getAndDiscreteUpdates.use:
              _._2.unNone.take(1).compile.drain.map: _ =>
                " due to fresh kill signal"
          .flatMap:
            case "" => IO.unit // stdouterrFiber terminated due to stdout and stderr EOF
            case stopReason => // Waiting cancelled
              logger.warn(s"Ignoring stdout and stderr${
                stopReason} (maybe a background child process is still running)")
              // Set stdouterrStopper in foreground, to be sure that no OrderStdWritten event is emitted.
              stdObservers.stdouterrStopper.stop(s"$label stopped$stopReason") *>
                // Cancellation may fail a child process writing to closed stdout or stderr with
                // EPIPE "Broken pipe".
                // Because InputStream may block (Linux), we cancel in the background.
                // See also .logWhenItTakesLonger in pumpStdouterrToSink
                stdouterrFiber.cancel
                  .startAndForget

  def sendProcessSignal(signal: ProcessSignal): IO[Unit] =
    kill(force = signal == SIGKILL)
      .guarantee:
        killSignal.update:
          case Some(SIGKILL) => Some(SIGKILL)
          case _ => Some(signal)

  private def kill(force: Boolean): IO[Unit] =
    if !force then
      processKiller.sigtermMainProcessAndSaveDescendant(process)
    else
      // Kill saved descendants, too
      processKiller.sigkillWithDescendants(process)

  @TestOnly
  private[process] def isAlive = process.isAlive

  override def toString = label


object PipedProcess:

  /** Grace period between SIGKILL and (second) destroyForcibly. */
  private val KillStdouterrDelay = 500.ms
  private val StdouterrWorry = Worry(
    List(1.s, 3.s, 6.s) ::: AfterTenSecondsWorryDurations,
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
    // TODO Make ResourceIO[PipedProcess]
    IO.defer:
      val commandArgs = toShellCommandArguments(commandLine.file, commandLine.arguments.tail)
      // Check argsToCommandLine here to avoid exception in WindowsProcess.start
      startProcess(commandArgs, conf, orderId)
        .flatMapT: process =>
          ProcessMXBean.starts += 1
          ProcessMXBean.running += 1
          val since = Deadline.now
          process.stdin.close() // Process gets an empty stdin
          for
            killSignal <- SignallingRef[IO].of(none[ProcessSignal])
            killer <- SubagentProcessKiller.resource(s"$orderId $process").toAllocated
          yield
            Right:
              PipedProcess(conf, process, stdObservers, orderId, jobKey, killSignal, killer, since)

  private def startProcess(args: Seq[String], conf: ProcessConfiguration, orderId: OrderId)
  : IO[Checked[Js7Process]] =
    conf.windowsLogon match
      case None =>
        val processBuilder = new ProcessBuilder(args.asJava)
        for o <- conf.workingDirectory do processBuilder.directory(o.toFile)

        transferEnv(from = conf.additionalEnvironment, to = processBuilder.environment)

        processBuilder.startRobustly(label = orderId.toString)
          .map(o => Right(JavaProcess(o)))

      case Some(logon) =>
        IO.blocking:
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
    from.collect { case (k, None) => k }.foreach(to.remove)
    to.putAll:
      from.collect { case (k, Some(v)) => k -> v }.asJava


  sealed trait ProcessMXBean:
    this: ProcessMXBean.type =>
    def getRunning: Int = running.get
    def getStarts: Long = starts.get

  object ProcessMXBean extends ProcessMXBean:
    private[PipedProcess] val running = Atomic(0)
    private[PipedProcess] val starts = Atomic(0L)
