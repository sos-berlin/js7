package js7.launcher.process

import cats.effect.IO
import cats.syntax.foldable.*
import cats.syntax.traverse.*
import js7.base.io.process.Processes.*
import js7.base.io.process.{Js7Process, Pid}
import js7.base.log.Logger
import js7.base.log.Logger.syntax.*
import js7.base.utils.ScalaUtils.syntax.*
import js7.launcher.process.ProcessKiller.*
import scala.jdk.OptionConverters.*
import scala.jdk.StreamConverters.*

/**
 * @tparam P denotes the main process (the order's process). */
trait ProcessKiller[P <: Pid | Js7Process]:

  private val logger = Logger.withPrefix[this.type](label)

  protected def label: String

  protected def dontExecute: Boolean = false

  def killMainProcessOnly(process: P, force: Boolean): IO[Unit]

  protected def killingLogLine(
    processHandle: ProcessHandle,
    force: Boolean,
    isDescendant: Boolean = false,
    isLastDescendant: Boolean = false)
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

  private def killAll(force: Boolean, processesAndDescs: Seq[(P, Seq[ProcessHandle])])
  : IO[Unit] =
    processesAndDescs
      .traverse: (process, descendants) =>
        val last = descendants.size - 1
        killMainProcessOnly(process, force = force) *>
          descendants.zipWithIndex
            .traverse: (h, i) =>
              killViaProcessHandle(h, force = force,
                isDescendant = true, isLastDescendant = i == last)
            .map(_.combineAll)
      .map(_.combineAll)

  private def trySendStopSignal(pids: Seq[Long]): IO[Unit] =
    sendStopSignal(pids)
      .handleErrorWith: t =>
        IO(logger.warn(t.toStringWithCauses))

  private def sendStopSignal(pids: Seq[Long]): IO[Unit] =
    IO.defer:
      val firstArgs = Vector("/bin/kill", "-STOP")
      val args = firstArgs ++ pids.map(_.toString)
      logger.info(args.mkString(" "))
      runAndLogProcess(args): line =>
        IO(logger.warn(s"${firstArgs.mkString(" ")}: $line"))
      .flatMap(returnCode => IO:
        if !returnCode.isSuccess then
          logger.warn(s"${firstArgs.mkString(" ")} exited with $returnCode"))

  protected final def killViaProcessHandle(
    processHandle: ProcessHandle,
    force: Boolean,
    isDescendant: Boolean = false,
    isLastDescendant: Boolean = false)
  : IO[Unit] =
    IO:
      logger.info(killingLogLine(processHandle, force, isDescendant, isLastDescendant))
      if !dontExecute then
        if force then
          processHandle.destroyForcibly()
        else
          processHandle.destroy()

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


object ProcessKiller:

  extension (process: Pid | Js7Process)
    private def toPid: Pid =
      process match
        case pid: Pid => pid
        case process: Js7Process => process.pid

    private def isAlive: Boolean =
      process match
        case pid: Pid => pid.maybeProcessHandle.exists(_.isAlive)
        case process: Js7Process => process.isAlive

    def descendants: Seq[ProcessHandle] =
      process.maybeProcessHandle.fold(Nil):
        _.descendants.toScala(Vector)

    def maybeProcessHandle: Option[ProcessHandle] =
      process match
        case pid: Pid => ProcessHandle.of(pid.number).toScala
        case process: Js7Process => process.maybeHandle
