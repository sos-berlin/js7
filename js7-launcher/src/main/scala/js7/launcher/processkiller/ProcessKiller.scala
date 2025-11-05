package js7.launcher.processkiller

import cats.effect.IO
import js7.base.catsutils.CatsExtensions.traverseCombine
import js7.base.catsutils.{Environment, FiberVar}
import js7.base.eventbus.EventPublisher
import js7.base.io.process.ProcessExtensions.{isAlive, maybeProcessHandle, onExitIO, toPid}
import js7.base.io.process.Processes.*
import js7.base.io.process.{JavaProcess, Js7Process, Pid}
import js7.base.log.Logger
import js7.base.log.Logger.syntax.*
import js7.base.metering.CallMeter
import js7.base.system.OperatingSystem.isUnix
import js7.base.time.ScalaTime.*
import js7.base.utils.Assertions.assertThat
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.utils.Tests.isStrict
import js7.launcher.processkiller.ProcessKiller.*
import scala.collection.mutable
import scala.concurrent.duration.Deadline
import scala.jdk.CollectionConverters.*
import scala.jdk.OptionConverters.*
import scala.jdk.StreamConverters.*

/**
 * @tparam P denotes the main process (the order's process). */
private[launcher] trait ProcessKiller[P <: Pid | Js7Process]:

  private val logger = Logger.withPrefix[this.type](label)

  /** Remember the descendant processes just before SIGTERM. */
  protected val sigtermDescendantsWatch: FiberVar[Unit]
  private var _sigtermDescendants: Map[Pid, Vector[ProcessHandle]] = Map.empty
  private val isTerminatedMainPid = mutable.Set[Pid]()

  protected def label: String

  protected def dontExecute: Boolean = false

  protected def killingLogLine(
    processHandle: ProcessHandle,
    force: Boolean,
    isDescendant: Boolean = false,
    isLast: Boolean = false)
  : String

  protected final def sigtermMainProcessesAndSaveDescendants(processes: Seq[P]): IO[Unit] =
    saveSigtermDescendants:
      processes.map(p => p.toPid -> p.descendants).toMap
    .productR:
      processes.traverseCombine:
        killMainProcessOnly(_, force = false)

  private def saveSigtermDescendants(descendants: Map[Pid, Vector[ProcessHandle]]): IO[Unit] =
    IO.defer:
      _sigtermDescendants = descendants // We will sigkill them later
      sigtermDescendantsWatch.startFiber:
        watchSigtermDescendants

  /** Watch for early termination of remembered descendant processes.
   * <p>
   * We watch the _sigtermDescendants for termination to avoid killing a PID which has
   * terminated long ago and is being reused by an other alien process. */
  private def watchSigtermDescendants: IO[Unit] =
    Environment.maybe[EventPublisher[TestChildProcessTerminated]].flatMap: testBus =>
      IO.defer:
        IO.whenA(_sigtermDescendants.nonEmpty):
          val handlesWithOwner =
            _sigtermDescendants.toVector.flatMap: (pid, descendants) =>
              (pid.maybeProcessHandle ++: descendants).map(_ -> pid)
          logger.debugIO("watchSigtermDescendants", handlesWithOwner.map(_._1.pid).mkString(" ")):
            fs2.Stream.iterable(handlesWithOwner)
              .covary[IO]
              .parEvalMapUnorderedUnbounded: (h, owner) =>
                h.onExitIO.as(h -> owner)
              .evalTap((h, owner) => IO:
                val pid = Pid(h.pid)
                if pid == owner then
                  isTerminatedMainPid += pid
                  logger.info(s"Process $pid terminated")
                else
                  _sigtermDescendants = _sigtermDescendants.updatedWith(owner):
                    case Some(descs) => Some(descs.filter(_ != h))
                    case None =>
                      logger.warn(s"Internal problem: Unknown watched process $pid")
                      None
                  logger.info(s"Descendant process $pid terminated")
                  testBus.foreach(_.publish(TestChildProcessTerminated(pid))))
              .compile.drain
              .productR:
                IO:
                  if isStrict then assertThat(_sigtermDescendants.forall(_._2.isEmpty))
                  _sigtermDescendants = Map.empty

  private def killMainProcessOnly(process: P, force: Boolean): IO[Unit] =
    IO.defer:
      IO.unlessA(isTerminatedMainPid(process.toPid)):
        process match
          case process: JavaProcess =>
            // Kill via ProcessHandle because this doesn't close stdout and stderr
            killProcessHandle(process.handle, force = force)

          case process: Js7Process =>
            killWithJava(process, force)

          case pid: Pid =>
            IO.defer:
              ProcessHandle.of(pid.number).toScala.fold(IO.unit): h =>
                killProcessHandle(h, force = force)

  protected final def sigkillWithDescendants(processes: Seq[P]): IO[Unit] =
    logger.debugIO:
      IO.defer:
        val isAlive = processes.filter(p => !isTerminatedMainPid(p.toPid)).filter(_.isAlive).toSet
        val processToDescendants: Seq[(P, Seq[ProcessHandle])] =
          processes.map: process =>
            // if process isAlive then use its current descendants
            // otherwise use the descendants as before SIGTERM.
            val descendants =
              isAlive(process).thenMaybe:
                // Use the current descendants of still alive process
                try Some:
                  process.descendants
                catch case e: RuntimeException =>
                  logger.error(s"Cannot query current descendants of ${process.toPid}: $e")
                  None
              .getOrElse:
                // Use the descendants as before SIGTERM.
                _sigtermDescendants.get(process.toPid).fold_(Vector.empty, descendantsOf)
            process -> descendants

        val stopPids = processToDescendants.flatMap: (process, descendants) =>
          (isAlive(process) ? process.toPid.number) ++: descendants.map(_.pid)

        // Try to stop all processes, then kill them
        IO.whenA(isUnix):
          trySendStopSignal(stopPids)
        *> sigkillAll(processToDescendants)

  /** Return a Seq of distinct ProcessHandles */
  private def descendantsOf(processHandles: Seq[ProcessHandle]): Seq[ProcessHandle] =
    val known = mutable.Set[Long]()
    val result = Vector.newBuilder[ProcessHandle]
    for p <- processHandles do
      if !known(p.pid) && p.isAlive then
        known += p.pid
        result += p
        for d <- p.descendantsIterator do
          if !known(d.pid) then
            known += d.pid
            result += d
    result.result()

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

  private def sigkillAll(processesAndDescs: Seq[(P, Seq[ProcessHandle])])
  : IO[Unit] =
    processesAndDescs
      .traverseCombine: (process, descendants) =>
        killMainProcessOnly(process, force = true) *>
          killDescendants(force = true)(descendants)

  private def killDescendants(force: Boolean)(descendants: Seq[ProcessHandle])
  : IO[Unit] =
    val last = descendants.size - 1
    descendants.zipWithIndex
      .traverseCombine: (h, i) =>
        killProcessHandle(h, force = force, isDescendant = true, isLast = i == last)

  private def killProcessHandle(
    processHandle: ProcessHandle,
    force: Boolean,
    isDescendant: Boolean = false,
    isLast: Boolean = false)
  : IO[Unit] =
    IO.defer:
      logger.info:
        killingLogLine(processHandle, force = force, isDescendant = isDescendant, isLast = isLast)
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
  private def killWithJava(process: Js7Process, force: Boolean): IO[Unit] =
    IO:
      if force then
        logger.info("◼️  destroyForcibly (SIGKILL)")
        if !dontExecute then
          process.destroyForcibly()
      else
        logger.info("◼️  destroy (SIGTERM)")
        if !dontExecute then
          process.destroy()


  extension (process: Pid | Js7Process)
    protected def descendants: Vector[ProcessHandle] =
      process.maybeProcessHandle.fold(Vector.empty)(_.descendantsVector)

  extension (processHandle: ProcessHandle)
    private def descendantsVector: Vector[ProcessHandle] =
      val t = Deadline.now
      val r =
        meterProcessHandleDescendant:
          processHandle.descendants.toScala(Vector)
      val elapsed = t.elapsed
      if elapsed >= 1.ms then
        logger.trace:
          s"ProcessHandle(PID:${processHandle.pid}).descendants => ${r.size} PIDs (${elapsed.pretty})"
      r

    private def descendantsIterator: Iterator[ProcessHandle] =
      val t = Deadline.now
      val r = meterProcessHandleDescendant:
        processHandle.descendants.iterator.asScala
      val elapsed = t.elapsed
      if elapsed >= 1.ms then
        logger.trace(s"ProcessHandle(PID:${processHandle.pid}).descendants (${elapsed.pretty})")
      r


object ProcessKiller:

  private val meterProcessHandleDescendant = CallMeter("ProcessHandle.descendants")
  final case class TestChildProcessTerminated(pid: Pid)
