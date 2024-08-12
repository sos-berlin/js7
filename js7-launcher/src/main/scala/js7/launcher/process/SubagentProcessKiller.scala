package js7.launcher.process

import cats.effect.{IO, Resource, ResourceIO}
import js7.base.catsutils.{Environment, FiberVar}
import js7.base.eventbus.EventPublisher
import js7.base.io.process.{JavaProcess, Js7Process, Pid}
import js7.base.log.Logger
import js7.base.log.Logger.syntax.*
import js7.base.time.ScalaTime.*
import js7.base.utils.ScalaUtils.*
import js7.base.utils.ScalaUtils.syntax.*
import js7.launcher.process.SubagentProcessKiller.*
import scala.collection.immutable.VectorBuilder
import scala.collection.mutable
import scala.concurrent.duration.Deadline
import scala.jdk.CollectionConverters.*
import scala.jdk.OptionConverters.*

private final class SubagentProcessKiller private(
  sigtermDescendantsWatch: FiberVar[Unit],
  protected val label: String)
extends ProcessKiller[Js7Process]:

  private val logger = Logger.withPrefix[this.type](label)

  /** Remember the descendant processes just before SIGTERM. */
  private var sigtermDescendants: Vector[ProcessHandle] = Vector.empty
  private var sigtermDescendantsSince = Deadline(ZeroDuration)

  def killMainProcessOnly(process: Js7Process, force: Boolean): IO[Unit] =
    IO.defer:
      IO.whenA(process.isAlive):
        IO.unlessA(force):
          IO:
            sigtermDescendants = process.descendants // We will sigkill them later
            sigtermDescendantsSince = Deadline.now
          .productR:
            sigtermDescendantsWatch.startFiber:
              watchSigtermDescendants
        .productR:
          process match
            case process: JavaProcess =>
              // Kill via ProcessHandle because this doesn't close stdout and stderr
              killViaProcessHandle(process.handle, force = force)
            case _ =>
              killWithJava(process, force)

  /** Watch for early termination of remembered descendant processes.
   * <p>
   * We watch the sigtermDescendants for termination to avoid killing a PID which has
   * terminated long ago and is being reused by an ther alien process. */
  private def watchSigtermDescendants: IO[Unit] =
    logger.debugIO:
      Environment.maybe[EventPublisher[TestChildProcessTerminated]].flatMap: testBus =>
        IO.defer:
          fs2.Stream.iterable(sigtermDescendants)
            .covary[IO]
            .parEvalMapUnorderedUnbounded: h =>
              IO.defer:
                logger.trace(s"Descendant process ${Pid(h.pid)} onExit ...")
                h.onExitIO.as(h)
            .evalTap(h => IO:
              val pid = Pid(h.pid)
              logger.debug(s"Descendant process $pid terminated")
              sigtermDescendants = sigtermDescendants.filter(_ != h)
              testBus.foreach(_.publish(TestChildProcessTerminated(pid))))
            .compile.drain
      *> IO.never

  def sigkillWithDescendants(process: Js7Process): IO[Unit] =
    IO.defer:
      // if process isAlive then use its current descendants
      // otherwise use the descendants as before SIGTERM.
      val descendants =
        if process.isAlive then
          val d = process.descendants
          if process.isAlive then // Still alive?
            d
          else
            descendantsOf(sigtermDescendants)
        else
          descendantsOf(sigtermDescendants)

      genericSigkillWithDescendants:
        Seq:
          process -> descendants

  protected def killingLogLine(
    processHandle: ProcessHandle,
    force: Boolean,
    isDescendant: Boolean,
    isLast: Boolean)
  : String =
    (if isDescendant then "❌ " else "⚫️ ") +
      (if force then """destroyForcibly (SIGKILL) """ else """destroy (SIGTERM) """) +
      (isDescendant ?? (
        "child process " +
          Pid(processHandle.pid) +
          ' ' +
          processHandle.info.commandLine.toScala.getOrElse("")))

  /** Return a Seq of distinct ProcessHandles */
  private def descendantsOf(processHandles: Seq[ProcessHandle]): Seq[ProcessHandle] =
    val known = mutable.Set[Long]()
    val result = VectorBuilder[ProcessHandle]()
    for p <- processHandles if p.isAlive do
      if !known(p.pid) then
        known += p.pid
        result += p
        for d <- p.descendants().iterator().asScala do
          if !known(d.pid) then
            known += d.pid
            result += d
    result.result()


object SubagentProcessKiller:
  final case class TestChildProcessTerminated(pid: Pid)

  def resource(label: String): ResourceIO[SubagentProcessKiller] =
    for
      fiberVar <- FiberVar.resource[Unit]
      killer <- Resource.eval(IO:
        SubagentProcessKiller(fiberVar, label))
    yield
      killer
