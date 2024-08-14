package js7.launcher.processkiller

import cats.effect.IO
import cats.syntax.foldable.*
import cats.syntax.parallel.*
import cats.syntax.traverse.*
import js7.base.io.process.Processes.*
import js7.base.io.process.{Js7Process, Pid, ReturnCode}
import js7.base.log.Logger
import js7.base.log.Logger.syntax.*
import js7.base.thread.IOExecutor.env.interruptibleVirtualThread
import js7.base.utils.ScalaUtils.syntax.*
import scala.jdk.OptionConverters.*
import scala.jdk.StreamConverters.*

/**
 * @tparam P denotes the main process (the order's process). */
private[launcher] trait ProcessKiller[P <: Pid | Js7Process]:

  private val logger = Logger.withPrefix[this.type](label)

  protected def label: String

  protected def dontExecute: Boolean = false

  protected def killMainProcessOnly(process: P, force: Boolean): IO[Unit]

  protected def killingLogLine(
    processHandle: ProcessHandle,
    force: Boolean,
    isDescendant: Boolean = false,
    isLast: Boolean = false)
  : String

  protected final def genericSigkillWithDescendants(processesAndDescs: Seq[(P, Seq[ProcessHandle])])
  : IO[Unit] =
    logger.debugIO:
      IO.defer:
        val processes = processesAndDescs.map(_._1)
        if processesAndDescs.map(_._2).forall(_.isEmpty) then
          processes
            .traverse:
              killMainProcessOnly(_, force = true)
            .map(_.combineAll)
        else
          // Try to stop all processes, then collect descendants again and kill them
          val pids = processesAndDescs.flatMap: (process, descendants) =>
            (process.isAlive ? process.toPid.number) ++: descendants.map(_.pid)
          trySendStopSignal(pids) *>
            killAll(force = true, processesAndDescs)

  private def trySendStopSignal(pids: Seq[Long]): IO[Unit] =
    sendStopSignal(pids)
      .handleErrorWith: t =>
        IO(logger.warn(t.toStringWithCauses))

  private def sendStopSignal(pids: Seq[Long]): IO[Unit] =
    IO.whenA(pids.nonEmpty):
      IO.defer:
        val firstArgs = Vector("/bin/kill", "-STOP")
        val args = firstArgs ++ pids.map(_.toString)
        logger.info(args.mkString(" "))
        runAndLogProcess(args): line =>
          IO(logger.warn(s"${firstArgs.mkString(" ")} >> $line"))
        .flatMap(returnCode => IO:
          if !returnCode.isSuccess then
            logger.warn(s"${firstArgs.mkString(" ")} exited with $returnCode"))

  private def killAll(force: Boolean, processesAndDescs: Seq[(P, Seq[ProcessHandle])])
  : IO[Unit] =
    processesAndDescs
      .traverse: (process, descendants) =>
        killMainProcessOnly(process, force = force) *>
          killDescendants(descendants, force = force)
      .map(_.combineAll)

  protected final def killDescendants(descendants: Seq[ProcessHandle], force: Boolean)
  : IO[Unit] =
    val last = descendants.size - 1
    descendants.zipWithIndex
      // in parallel, because onExit for each process take a little time
      .parTraverse: (h, i) =>
        killProcessHandle(h, force = force, isDescendant = true, isLast = i == last)
      .map(_.combineAll)

  protected final def killProcessHandle(
    processHandle: ProcessHandle,
    force: Boolean,
    isDescendant: Boolean = false,
    isLast: Boolean = false)
  : IO[Unit] =
    IO.defer:
      logger.info(killingLogLine(processHandle, force, isDescendant, isLast))
      IO.unlessA(dontExecute):
        if force then
          IO:
            val ok = processHandle.destroyForcibly()
            if !ok then logger.debug(s"⚠️ destroyForcibly ${Pid(processHandle.pid)} returned false")
            //avoidZombie(processHandle)
        else
          IO(processHandle.destroy())

  // Try to eliminate zombies under AlmaLinux 9:
  //private final def avoidZombie(processHandle: ProcessHandle): IO[Unit] =
  //  val pid = Pid(processHandle.pid)
  //  val timeout = 3.s // ???
  //  logger.traceIO(s"avoidZombie $pid"):
  //    processHandle.onExitIO
  //  //.logWhenItTakesLonger(s"termination of SIGKILLed $pid")
  //  .timeoutTo(timeout, IO:
  //    logger.warn(s"$pid has not terminated despite SIGKILL ${timeout.pretty} ago"))

  // Used only by SubagentProcessKiller subclass,
  // but we want to use the ProcessKiller.logger for all logging.
  protected final def killWithJava(process: Js7Process, force: Boolean): IO[Unit] =
    IO:
      if force then
        logger.info("⚫️ destroyForcibly (SIGKILL)")
        if !dontExecute then
          process.destroyForcibly()
      else
        logger.info("⚫️ destroy (SIGTERM)")
        if !dontExecute then
          process.destroy()

  final def waitForReturnCode(process: Js7Process): IO[ReturnCode] =
    // Try onExit to avoid blocking a (virtual) thread
    process.maybeHandle.fold(IO.unit)(_.onExitIO) *>
      interruptibleVirtualThread:
        logger.traceCallWithResult(s"waitFor $process"):
          process.waitFor()


  extension (process: Pid | Js7Process)
    private def toPid: Pid =
      process match
        case pid: Pid => pid
        case process: Js7Process => process.pid

    protected def isAlive: Boolean =
      process match
        case pid: Pid => pid.maybeProcessHandle.exists(_.isAlive)
        case process: Js7Process => process.isAlive

    //def untilTerminated: IO[Unit] =
    //  process.maybeProcessHandle match
    //    case Some(h) =>
    //      h.onExitIO // Avoid blocking in Js7Process#waitFor
    //    case None => process match
    //      case _: Pid =>
    //        IO.unit
    //      case process: Js7Process =>
    //        waitForReturnCode(process).void

    def descendants: Vector[ProcessHandle] =
      process.maybeProcessHandle.fold(Vector.empty):
        _.descendants.toScala(Vector)

    def maybeProcessHandle: Option[ProcessHandle] =
      process match
        case pid: Pid => ProcessHandle.of(pid.number).toScala
        case process: Js7Process => process.maybeHandle

  //extension (process: Js7Process)
  //  def untilTerminated: IO[Unit] =
  //    process
  //      .maybeHandle.fold(IO.unit):
  //        _.untilTerminated // Avoid blocking in Js7Process#waitFor
  //      .productR:
  //        waitForReturnCode(process).void

  extension (processHandle: ProcessHandle)
    def onExitIO: IO[Unit] =
      IO.fromCompletableFuture(IO:
        processHandle.onExit())
      .void
